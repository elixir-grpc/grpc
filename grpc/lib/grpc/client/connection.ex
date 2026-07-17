defmodule GRPC.Client.Connection do
  @moduledoc """
  Connection manager for gRPC client channels, with optional load balancing
  and name resolution support.

  A `Conn` process manages one or more underlying gRPC connections
  (`GRPC.Channel` structs) and exposes a virtual channel to be used by
  client stubs. The orchestration process runs as a `GenServer` registered
  in a node-local `Registry`, so named channels are scoped to the current
  BEAM node.

  ## Overview

  * `connect/2` – establishes a client connection (single or multi-channel).
  * `pick/2` – chooses a channel according to the active load-balancing policy.
  * `disconnect/1` – gracefully closes a connection and frees resources.

  Under the hood:

  * The target string is resolved using a [Resolver](GRPC.Client.Resolver).
  * Depending on the target and service config, a load-balancing module is chosen
    (e.g. `PickFirst`, `RoundRobin`).
  * Each call to `pick/2` dispatches to the LB module, which selects a channel
    per request. DNS re-resolution reconciles the LB's channel list in place.

  ## Supervised connections

  For long-running clients, declare the connection in your own supervision
  tree instead of calling `connect/2` at runtime. In that mode, establishment
  happens asynchronously after process start: if the backend is unreachable,
  the process stays alive and retries with exponential backoff instead of
  failing supervisor startup. Use `await_ready/2` to block until the channel
  is usable.

      children = [
        {GRPC.Client.Connection,
         name: MyApp.PaymentsConnection,
         target: "dns://payments.internal:50051",
         lb_policy: :round_robin}
      ]

      channel = GRPC.Client.Connection.get_channel!(MyApp.PaymentsConnection)
      Payments.Stub.charge(channel, request)

  Or define a connection module and configure it through the application
  environment:

      defmodule MyApp.PaymentsConnection do
        use GRPC.Client.Connection, otp_app: :my_app
      end

      # config/runtime.exs
      config :my_app, MyApp.PaymentsConnection,
        target: "dns://payments.internal:50051",
        lb_policy: :round_robin

      # in your supervision tree
      children = [MyApp.PaymentsConnection]

      channel = GRPC.Client.Connection.get_channel!(MyApp.PaymentsConnection)
      Payments.Stub.charge(channel, request)

  Options are merged in this order (later wins): options given to
  `use GRPC.Client.Connection`, then the application environment, then
  options passed to `start_link/1`.

  The channel handle returned by `get_channel/1` is valid as soon as the
  process is running, even while the connection is still being established;
  RPCs return `{:error, :no_connection}` style errors until then.

  `connect/2` keeps its historical fail-fast contract: it blocks until the
  first establishment attempt finishes and returns `{:error, reason}` (tearing
  the process down) if that attempt fails.

  ## Target syntax

  The `target` argument to `connect/2` accepts URI-like strings that are resolved
  via the configured `Resolver` (default `GRPC.Client.Resolver`).

  Examples of supported formats:

    * `"dns://example.com:50051"`
    * `"ipv4:10.0.0.5:50051"`
    * `"unix:/tmp/my.sock"`
    * `"xds:///my-service"`
    * `"127.0.0.1:50051"` (implicit DNS / fallback to IPv4)

  See [`GRPC.Client.Resolver`](GRPC.Client.Resolver) for the full specification.

  ## Examples

  ### Basic connect and RPC

      iex> opts = [adapter: GRPC.Client.Adapters.Gun]
      iex> {:ok, ch} = GRPC.Client.Connection.connect("127.0.0.1:50051", opts)
      iex> req = %Grpc.Testing.SimpleRequest{response_size: 42}
      iex> {:ok, resp} = Grpc.Testing.TestService.Stub.unary_call(ch, req)
      iex> resp.response_size
      42

  ### Using interceptors and custom adapter

      iex> opts = [interceptors: [GRPC.Client.Interceptors.Logger],
      ...>         adapter: GRPC.Client.Adapters.Mint]
      iex> {:ok, ch} = GRPC.Client.Connection.connect("dns://my-service.local:50051", opts)
      iex> {:ok, channel} = GRPC.Client.Connection.pick(ch)
      iex> channel.host
      "127.0.0.1"

  ### Unix socket target

      iex> {:ok, ch} = GRPC.Client.Connection.connect("unix:/tmp/service.sock")
      iex> Grpc.Testing.TestService.Stub.empty_call(ch, %{})

  ### Disconnect

      iex> {:ok, ch} = GRPC.Client.Connection.connect("127.0.0.1:50051")
      iex> GRPC.Client.Connection.disconnect(ch)
      {:ok, %GRPC.Channel{...}}

  """
  use GenServer
  alias GRPC.Channel
  alias GRPC.Client.Connection.EndpointResolver

  require Logger

  @insecure_scheme "http"
  @secure_scheme "https"
  @default_resolve_interval 30_000
  @default_max_resolve_interval 300_000
  @default_min_resolve_interval 5_000
  @default_connect_timeout 15_000
  @backoff_initial 1_000
  @backoff_multiplier 1.6
  @backoff_max 120_000
  @backoff_jitter 0.2

  @type t :: %__MODULE__{
          virtual_channel: Channel.t(),
          real_channels: %{String.t() => {:connected, Channel.t()} | {:failed, any()}},
          lb_mod: module() | nil,
          lb_state: term() | nil,
          resolver: module() | nil,
          adapter: module(),
          resolver_target: String.t() | nil,
          connect_opts: keyword(),
          resolver_state: term() | nil,
          established?: boolean(),
          last_error: term() | nil,
          retry_attempt: non_neg_integer(),
          waiters: [{pid(), GenServer.from(), reference()}]
        }

  defstruct virtual_channel: nil,
            real_channels: %{},
            lb_mod: nil,
            lb_state: nil,
            resolver: nil,
            adapter: GRPC.Client.Adapters.Gun,
            resolver_target: nil,
            connect_opts: [],
            resolver_state: nil,
            established?: false,
            last_error: nil,
            retry_attempt: 0,
            waiters: []

  @doc """
  Returns a child spec for running a connection under a supervisor.

  Accepts the same options as `connect/2` plus the required `:target` and
  `:name` keys. The spec only captures the target and options, so a restarted
  process re-resolves and re-dials from scratch instead of reusing stale
  state.
  """
  def child_spec({target, opts}) do
    opts = Keyword.put_new_lazy(opts, :name, &make_ref/0)

    %{
      id: {__MODULE__, opts[:name]},
      start: {__MODULE__, :start_link, [target, opts]},
      restart: :transient,
      type: :worker,
      shutdown: 5000
    }
  end

  def child_spec(opts) when is_list(opts) do
    child_spec(pop_target!(opts))
  end

  @doc """
  Starts a connection process linked to the caller.

  Accepts the same options as `child_spec/1`. Establishment (resolution and
  dialing) happens asynchronously after the process starts; use
  `await_ready/2` to block until the channel is usable.
  """
  def start_link(opts) when is_list(opts) do
    {target, opts} = pop_target!(opts)
    start_link(target, opts)
  end

  def start_link(target, opts) do
    opts = Keyword.put_new_lazy(opts, :name, &make_ref/0)
    GenServer.start_link(__MODULE__, {target, opts}, name: via(opts[:name]))
  end

  @doc false
  # Runtime entry point for `use`-based connection modules.
  def start_link(module, otp_app, use_opts, opts) do
    use_opts
    |> Keyword.merge(Application.get_env(otp_app, module, []))
    |> Keyword.merge(opts)
    |> Keyword.put(:name, module)
    |> start_link()
  end

  @impl GenServer
  def init({target, opts}) do
    Process.flag(:trap_exit, true)

    state = build_initial_state(target, opts)

    :persistent_term.put(channel_key(state.virtual_channel.ref), state.virtual_channel)

    {:ok, state, {:continue, :establish}}
  end

  @doc """
  Establishes a new client connection to a gRPC server or set of servers.

  The `target` string determines how the endpoints are resolved
  (see [Resolver](GRPC.Client.Resolver)).

  Options:

    * `:adapter` – transport adapter module (default: `GRPC.Client.Adapters.Gun`)
    * `:adapter_opts` – options passed to the adapter
    * `:resolver` – resolver module (default: `GRPC.Client.Resolver`)
    * `:lb_policy` – load-balancing policy (`:pick_first`, `:round_robin`)
    * `:interceptors` – list of client interceptors
    * `:codec` – request/response codec (default: `GRPC.Codec.Proto`)
    * `:compressor` / `:accepted_compressors` – message compression
    * `:headers` – default metadata headers
    * `:connect_timeout` – how long `connect/2` waits for the first
      establishment attempt in ms (default: 15000)
    * `:resolve_interval` – DNS re-resolution interval in ms (default: 30000)
    * `:max_resolve_interval` – backoff cap in ms (default: 300000)
    * `:min_resolve_interval` – rate-limit floor in ms (default: 5000)

  Returns:

    * `{:ok, channel}` – a `GRPC.Channel` usable with stubs
    * `{:error, {:already_started_with_different_target, target}}` – the name
      is already registered to a connection with a different target
    * `{:error, reason}` – if connection fails

  ## Examples

      iex> {:ok, ch} = GRPC.Client.Connection.connect("127.0.0.1:50051")
      iex> Grpc.Testing.TestService.Stub.empty_call(ch, %{})
  """
  def connect(target, opts \\ []) do
    opts = Keyword.put_new_lazy(opts, :name, &make_ref/0)

    # Validate the target and options in the caller so configuration errors
    # raise here instead of crashing the spawned process. Interceptor init is
    # skipped here so it only runs once, in the connection process.
    validated = build_initial_state(target, opts, _init_interceptors? = false)

    timeout = Keyword.get(opts, :connect_timeout, @default_connect_timeout)
    name = opts[:name]

    case DynamicSupervisor.start_child(GRPC.Client.Supervisor, child_spec({target, opts})) do
      {:ok, pid} ->
        case ready_status(name, validated.resolver_target, timeout) do
          :ok ->
            finalize_connection(name)

          {:error, reason} ->
            _ = DynamicSupervisor.terminate_child(GRPC.Client.Supervisor, pid)
            {:error, reason}
        end

      {:error, {:already_started, _pid}} ->
        case ready_status(name, validated.resolver_target, timeout) do
          :ok -> finalize_connection(name)
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Blocks until the connection has at least one established channel.

  Returns `:ok` once established, or `{:error, :timeout}` if the connection
  is still retrying when `timeout` elapses. Unlike `connect/2`, a failed
  establishment attempt does not resolve the wait: the process keeps
  retrying with backoff and this call returns as soon as an attempt succeeds.

  Returns `{:error, :not_started}` if the connection is not running, or if it
  is disconnected or shut down while waiting.
  """
  def await_ready(ref_or_channel, timeout \\ 5_000)

  def await_ready(%Channel{ref: ref}, timeout), do: await_ready(ref, timeout)

  def await_ready(ref, timeout) do
    GenServer.call(via(ref), :await_ready, timeout)
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, {:noproc, _} -> {:error, :not_started}
    :exit, {:normal, _} -> {:error, :not_started}
    :exit, {:shutdown, _} -> {:error, :not_started}
    :exit, {{:shutdown, _}, _} -> {:error, :not_started}
    :exit, {reason, _} -> {:error, reason}
  end

  @doc """
  Returns the `GRPC.Channel` handle for a running named connection.

  The handle is a lightweight identity struct: stubs resolve it to a healthy
  underlying connection on every RPC, so it stays valid across reconnects and
  endpoint changes and can be fetched once and cached. It is readable as soon
  as the process is running, even while establishment is still in progress.

  Returns `{:error, :not_started}` if no connection with that name is running.
  """
  def get_channel(name) do
    case :persistent_term.get(channel_key(name), nil) do
      %Channel{} = channel -> {:ok, channel}
      nil -> {:error, :not_started}
    end
  end

  @doc """
  Same as `get_channel/1`, but raises if the connection is not running.
  """
  def get_channel!(name) do
    case get_channel(name) do
      {:ok, channel} ->
        channel

      {:error, :not_started} ->
        raise ArgumentError,
              "no gRPC connection named #{inspect(name)} is running, " <>
                "add `{GRPC.Client.Connection, name: #{inspect(name)}, target: ...}` " <>
                "to your supervision tree"
    end
  end

  @doc """
  Disconnects a channel previously returned by `connect/2`.

  This will close all underlying real connections for the orchestrator
  and stop its process.

  Returns `{:ok, channel}` on success.

  ## Example

      iex> {:ok, ch} = GRPC.Client.Connection.connect("127.0.0.1:50051")
      iex> GRPC.Client.Connection.disconnect(ch)
      {:ok, %GRPC.Channel{}}
  """
  def disconnect(%Channel{ref: ref} = channel) do
    GenServer.call(via(ref), {:disconnect, channel})
  end

  def disconnect(name) do
    with {:ok, channel} <- get_channel(name) do
      disconnect(channel)
    end
  end

  @doc """
  Picks a channel from the orchestrator according to the active
  load-balancing policy.

  Normally, you don’t need to call `pick/2` directly – client stubs do this
  automatically – but it can be useful when debugging or testing.

  Returns:

    * `{:ok, channel}` – the chosen `GRPC.Channel`
    * `{:error, :no_connection}` – if the orchestrator is not available

  ## Example

      iex> {:ok, ch} = GRPC.Client.Connection.connect("dns://my-service.local:50051")
      iex> GRPC.Client.Connection.pick(ch)
      {:ok, %GRPC.Channel{host: "192.168.1.1", port: 50051}}
  """
  def pick_channel(%Channel{ref: ref} = _channel, _opts \\ []) do
    case :persistent_term.get(lb_key(ref), nil) do
      {lb_mod, lb_state} when not is_nil(lb_mod) ->
        case lb_mod.pick(lb_state) do
          {:ok, %Channel{} = channel, _new_state} -> {:ok, channel}
          {:error, _} -> {:error, :no_connection}
        end

      _ ->
        {:error, :no_connection}
    end
  end

  @doc """
  Triggers an immediate DNS re-resolution, subject to rate limiting.

  Intended for use by health checks or heartbeat mechanisms that detect
  a backend has gone away and want to force a fresh DNS lookup.
  """
  def resolve_now(%Channel{ref: ref}) do
    GenServer.cast(via(ref), :resolve_now)
  end

  defmacro __using__(use_opts) do
    quote bind_quoted: [use_opts: use_opts] do
      @grpc_connection_otp_app Keyword.fetch!(use_opts, :otp_app)
      @grpc_connection_opts Keyword.delete(use_opts, :otp_app)

      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          restart: :transient,
          type: :worker,
          shutdown: 5000
        }
      end

      def start_link(opts \\ []) do
        GRPC.Client.Connection.start_link(
          __MODULE__,
          @grpc_connection_otp_app,
          @grpc_connection_opts,
          opts
        )
      end

      defoverridable child_spec: 1
    end
  end

  @impl GenServer
  def handle_continue(:establish, state), do: attempt_establish(state)

  def handle_continue(:stop, state) do
    Logger.debug("#{inspect(__MODULE__)} stopping as requested")
    {:stop, :normal, state}
  end

  @impl GenServer
  def handle_cast(:resolve_now, %{resolver: resolver, resolver_state: rs} = state)
      when not is_nil(rs) do
    {:ok, new_rs} = resolver.update(rs, :resolve_now)
    {:noreply, %{state | resolver_state: new_rs}}
  end

  def handle_cast(:resolve_now, state), do: {:noreply, state}

  @impl GenServer
  def handle_call({:ready_status, expected_target}, _from, state) do
    cond do
      expected_target != state.resolver_target ->
        {:reply, {:error, {:already_started_with_different_target, state.resolver_target}}, state}

      state.established? ->
        {:reply, :ok, state}

      true ->
        {:reply, {:error, state.last_error || :connecting}, state}
    end
  end

  def handle_call(:await_ready, _from, %__MODULE__{established?: true} = state) do
    {:reply, :ok, state}
  end

  def handle_call(:await_ready, {caller_pid, _tag} = from, state) do
    {:noreply, %{state | waiters: add_waiter(state.waiters, caller_pid, from)}}
  end

  def handle_call({:disconnect, %Channel{adapter: adapter} = channel}, _from, state) do
    if state.resolver_state && function_exported?(state.resolver, :shutdown, 1) do
      state.resolver.shutdown(state.resolver_state)
    end

    :persistent_term.erase(lb_key(channel.ref))
    :persistent_term.erase(channel_key(channel.ref))
    disconnect_real_channels(state.real_channels, adapter)

    reply_waiters(state.waiters, {:error, :not_started})

    resp = {:ok, %Channel{channel | adapter_payload: %{conn_pid: nil}}}

    state = %{
      state
      | established?: false,
        real_channels: %{},
        resolver_state: nil,
        waiters: []
    }

    {:reply, resp, state, {:continue, :stop}}
  end

  @impl GenServer
  def handle_info(:retry_establish, %__MODULE__{established?: true} = state) do
    {:noreply, state}
  end

  def handle_info(:retry_establish, state), do: attempt_establish(state)

  def handle_info({:resolver_update, result}, state) do
    state = handle_resolve_result(result, state)
    {:noreply, state}
  end

  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, state}

  def handle_info({:EXIT, pid, reason}, %{resolver: resolver, resolver_state: rs} = state)
      when not is_nil(rs) do
    # Adapter connection processes are linked too, so re-init must be gated
    # on the resolver worker's own pid: re-initializing on any linked exit
    # would spawn a duplicate worker and orphan the live one.
    if pid == resolver_worker_pid(rs) do
      Logger.warning("Resolver worker exited: #{inspect(reason)}, re-initializing")

      state =
        if function_exported?(resolver, :init, 2) do
          case resolver.init(state.resolver_target,
                 connection_pid: self(),
                 connect_opts: state.connect_opts
               ) do
            {:ok, new_rs} -> %{state | resolver_state: new_rs}
            {:error, _} -> %{state | resolver_state: nil}
          end
        else
          %{state | resolver_state: nil}
        end

      {:noreply, state}
    else
      Logger.warning(
        "#{inspect(__MODULE__)} received :EXIT from #{inspect(pid)} reason: #{inspect(reason)}"
      )

      {:noreply, state}
    end
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.warning(
      "#{inspect(__MODULE__)} received :EXIT from #{inspect(pid)} reason: #{inspect(reason)}"
    )

    {:noreply, state}
  end

  def handle_info({:DOWN, mon, :process, pid, reason}, state) do
    case Enum.split_with(state.waiters, fn {_pid, _from, m} -> m == mon end) do
      {[], _} ->
        Logger.warning(
          "#{inspect(__MODULE__)} received :DOWN from #{inspect(pid)} with reason: #{inspect(reason)}"
        )

        {:noreply, state}

      {_dropped, remaining} ->
        {:noreply, %{state | waiters: remaining}}
    end
  end

  def handle_info(msg, state) do
    Logger.warning("#{inspect(__MODULE__)} received unexpected message: #{inspect(msg)}")

    {:noreply, state}
  end

  @impl GenServer
  def terminate(reason, %__MODULE__{virtual_channel: %{ref: ref}} = state) do
    if state.resolver_state && function_exported?(state.resolver, :shutdown, 1) do
      state.resolver.shutdown(state.resolver_state)
    end

    disconnect_real_channels(state.real_channels, state.adapter)
    :persistent_term.erase(lb_key(ref))

    # On a crash the supervisor restarts the process and init/1 re-registers
    # the handle; erasing it here would make get_channel!/1 raise during the
    # restart window.
    if normal_shutdown?(reason) do
      :persistent_term.erase(channel_key(ref))
    end

    :ok
  rescue
    _ -> :ok
  end

  def terminate(_reason, _state), do: :ok

  defp pop_target!(opts) do
    {target, opts} = Keyword.pop(opts, :target)

    unless is_binary(target) do
      raise ArgumentError,
            "the :target option is required and must be a string, got: #{inspect(target)}"
    end

    unless Keyword.has_key?(opts, :name) do
      raise ArgumentError,
            "the :name option is required for supervised connections, " <>
              "e.g. `{GRPC.Client.Connection, name: MyApp.Connection, target: #{inspect(target)}}`"
    end

    {target, opts}
  end

  defp ready_status(ref, expected_target, timeout) do
    GenServer.call(via(ref), {:ready_status, expected_target}, timeout)
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, {:noproc, _} -> {:error, :not_started}
    :exit, {:normal, _} -> {:error, :not_started}
    :exit, {:shutdown, _} -> {:error, :not_started}
    :exit, {{:shutdown, _}, _} -> {:error, :not_started}
    :exit, {reason, _} -> {:error, reason}
  end

  defp attempt_establish(state) do
    case establish(state) do
      {:ok, established_state} ->
        reply_waiters(established_state.waiters, :ok)

        {:noreply,
         %{
           established_state
           | established?: true,
             waiters: [],
             retry_attempt: 0,
             last_error: nil
         }}

      {:error, reason} ->
        delay = backoff_delay(state.retry_attempt)

        Logger.warning(
          "Failed to establish gRPC connection to #{state.resolver_target}: " <>
            "#{inspect(reason)}, retrying in #{delay}ms"
        )

        Process.send_after(self(), :retry_establish, delay)
        {:noreply, %{state | last_error: reason, retry_attempt: state.retry_attempt + 1}}
    end
  end

  defp establish(%__MODULE__{} = state) do
    norm_opts = state.connect_opts
    adapter = state.adapter

    {addresses, lb_mod} =
      case state.resolver.resolve(state.resolver_target) do
        {:ok, %{addresses: addresses, service_config: config}} ->
          {addresses, choose_lb_mod(config, norm_opts[:lb_policy])}

        {:error, _reason} ->
          # Fall back to treating the target as a single direct endpoint. Any
          # LB policy would only have one address to choose from, so PickFirst
          # is the only meaningful choice.
          {host, port} = EndpointResolver.split_host_port(state.resolver_target)
          {[%{address: host, port: port}], GRPC.Client.LoadBalancing.PickFirst}
      end

    real_channels = build_real_channels(addresses, state.virtual_channel, norm_opts, adapter)

    case init_lb(lb_mod, real_channels, adapter) do
      {:ok, lb_state} ->
        resolver_state = maybe_init_resolver(state)
        :persistent_term.put(lb_key(state.virtual_channel.ref), {lb_mod, lb_state})

        {:ok,
         %__MODULE__{
           state
           | real_channels: real_channels,
             lb_mod: lb_mod,
             lb_state: lb_state,
             resolver_state: resolver_state
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp init_lb(lb_mod, real_channels, adapter) do
    case connected_channels(real_channels) do
      [] ->
        disconnect_real_channels(real_channels, adapter)
        {:error, first_failure(real_channels) || :no_addresses}

      connected ->
        case lb_mod.init(channels: connected) do
          {:ok, lb_state} ->
            {:ok, lb_state}

          {:error, reason} ->
            disconnect_real_channels(real_channels, adapter)
            {:error, reason}
        end
    end
  end

  defp first_failure(real_channels) do
    Enum.find_value(real_channels, fn
      {_key, {:failed, reason}} -> reason
      _ -> nil
    end)
  end

  defp maybe_init_resolver(%__MODULE__{resolver_state: rs}) when not is_nil(rs), do: rs

  defp maybe_init_resolver(%__MODULE__{} = state) do
    if Code.ensure_loaded?(state.resolver) and function_exported?(state.resolver, :init, 2) do
      case state.resolver.init(state.resolver_target,
             connection_pid: self(),
             connect_opts: state.connect_opts
           ) do
        {:ok, resolver_state} ->
          resolver_state

        {:error, reason} ->
          Logger.warning(
            "Failed to initialize resolver for #{state.resolver_target}: " <>
              "#{inspect(reason)}, background re-resolution disabled"
          )

          nil
      end
    else
      nil
    end
  end

  # A caller whose await_ready call already timed out leaves a stale entry
  # behind; replacing entries per caller pid and monitoring the caller keeps
  # the waiter list bounded during long outages.
  defp add_waiter(waiters, caller_pid, from) do
    {stale, rest} = Enum.split_with(waiters, fn {pid, _from, _mon} -> pid == caller_pid end)
    Enum.each(stale, fn {_pid, _from, mon} -> Process.demonitor(mon, [:flush]) end)
    [{caller_pid, from, Process.monitor(caller_pid)} | rest]
  end

  defp reply_waiters(waiters, reply) do
    Enum.each(waiters, fn {_pid, from, mon} ->
      Process.demonitor(mon, [:flush])
      GenServer.reply(from, reply)
    end)
  end

  defp backoff_delay(attempt) do
    base = min(@backoff_initial * :math.pow(@backoff_multiplier, attempt), @backoff_max)
    jitter = (:rand.uniform() * 2 - 1) * @backoff_jitter * base
    max(trunc(base + jitter), 0)
  end

  defp finalize_connection(ref) do
    case pick_channel(%Channel{ref: ref}) do
      {:ok, %Channel{} = channel} -> {:ok, channel}
      _ -> {:error, :no_connection}
    end
  end

  defp disconnect_real_channels(real_channels, adapter) when is_map(real_channels) do
    Enum.each(real_channels, fn
      {_key, {:connected, ch}} -> do_disconnect(adapter, ch)
      _ -> :ok
    end)
  end

  defp disconnect_real_channels(_real_channels, _adapter), do: :ok

  defp handle_resolve_result({:ok, %{addresses: []}}, state), do: state

  defp handle_resolve_result({:ok, %{addresses: new_addresses}}, state) do
    reconcile_channels(new_addresses, state.adapter, state.connect_opts, state)
  end

  defp handle_resolve_result({:error, _reason}, state), do: state

  defp reconcile_channels(new_addresses, adapter, opts, state) do
    new_keys = MapSet.new(new_addresses, &build_address_key(&1.address, &1.port))
    old_keys = MapSet.new(Map.keys(state.real_channels))

    added = MapSet.difference(new_keys, old_keys)
    removed = MapSet.difference(old_keys, new_keys)

    real_channels = disconnect_removed_channels(removed, adapter, state.real_channels)

    real_channels =
      connect_new_channels(new_addresses, added, adapter, opts, state, real_channels)

    rebalance_after_reconcile(real_channels, state)
  end

  defp disconnect_removed_channels(removed, adapter, real_channels) do
    Enum.reduce(MapSet.to_list(removed), real_channels, fn key, channels ->
      case Map.get(channels, key) do
        {:connected, ch} -> do_disconnect(adapter, ch)
        _ -> :ok
      end

      Map.delete(channels, key)
    end)
  end

  defp connect_new_channels(new_addresses, added, adapter, opts, state, real_channels) do
    Enum.reduce(new_addresses, real_channels, fn %{address: host, port: port}, channels ->
      key = build_address_key(host, port)
      existing = Map.get(channels, key)

      should_connect =
        MapSet.member?(added, key) or
          match?({:failed, _}, existing) or
          not channel_alive?(existing)

      if should_connect do
        case existing do
          {:connected, ch} -> do_disconnect(adapter, ch)
          _ -> :ok
        end

        case connect_real_channel(state.virtual_channel, host, port, opts, adapter) do
          {:ok, ch} -> Map.put(channels, key, {:connected, ch})
          {:error, reason} -> Map.put(channels, key, {:failed, reason})
        end
      else
        channels
      end
    end)
  end

  defp rebalance_after_reconcile(real_channels, state) do
    connected = connected_channels(real_channels)

    new_lb_state =
      if state.lb_mod do
        case reconcile_lb(state.lb_mod, state.lb_state, connected) do
          {:ok, s} -> s
          {:error, _} -> state.lb_state
        end
      else
        state.lb_state
      end

    if connected == [] do
      Logger.warning("No healthy channels available after re-resolution")
    end

    %{state | real_channels: real_channels, lb_state: new_lb_state}
  end

  defp reconcile_lb(lb_mod, lb_state, new_channels) do
    lb_mod.update(lb_state, new_channels)
  end

  defp connected_channels(real_channels) do
    for {_key, {:connected, ch}} <- real_channels, do: ch
  end

  defp channel_alive?({:connected, %{adapter_payload: %{conn_pid: pid}}}) when is_pid(pid) do
    Process.alive?(pid)
  end

  defp channel_alive?({:connected, _}), do: true
  defp channel_alive?(_), do: false

  defp via(ref) do
    {:via, Registry, {GRPC.Client.Registry, {__MODULE__, ref}}}
  end

  defp lb_key(ref), do: {__MODULE__, :lb, ref}
  defp channel_key(ref), do: {__MODULE__, :channel, ref}

  defp normal_shutdown?(:normal), do: true
  defp normal_shutdown?(:shutdown), do: true
  defp normal_shutdown?({:shutdown, _}), do: true
  defp normal_shutdown?(_), do: false

  defp resolver_worker_pid({_mod, %{worker_pid: pid}}) when is_pid(pid), do: pid
  defp resolver_worker_pid(%{worker_pid: pid}) when is_pid(pid), do: pid
  defp resolver_worker_pid(_), do: nil

  defp do_disconnect(adapter, channel) do
    adapter.disconnect(channel)
  rescue
    _ ->
      :ok
  catch
    _type, _value ->
      :ok
  end

  defp build_initial_state(target, opts, init_interceptors? \\ true) do
    opts =
      Keyword.validate!(opts,
        cred: nil,
        name: make_ref(),
        adapter: GRPC.Client.Adapters.Gun,
        adapter_opts: [],
        interceptors: [],
        codec: GRPC.Codec.Proto,
        compressor: nil,
        accepted_compressors: [],
        headers: [],
        lb_policy: nil,
        connect_timeout: @default_connect_timeout,
        resolver: GRPC.Client.Resolver,
        resolve_interval: @default_resolve_interval,
        max_resolve_interval: @default_max_resolve_interval,
        min_resolve_interval: @default_min_resolve_interval
      )

    resolver = Keyword.get(opts, :resolver, GRPC.Client.Resolver)
    adapter = Keyword.get(opts, :adapter, GRPC.Client.Adapters.Gun)

    validate_adapter_opts!(opts[:adapter_opts])

    {norm_target, norm_opts, scheme} = normalize_target_and_opts(target, opts)
    cred = resolve_credential(norm_opts[:cred], scheme)

    interceptors =
      if init_interceptors?, do: init_interceptors(norm_opts[:interceptors]), else: []

    accepted_compressors =
      build_compressor_list(norm_opts[:compressor], norm_opts[:accepted_compressors])

    virtual_channel = %Channel{
      scheme: scheme,
      cred: cred,
      ref: opts[:name],
      adapter: adapter,
      interceptors: interceptors,
      codec: norm_opts[:codec],
      compressor: norm_opts[:compressor],
      accepted_compressors: accepted_compressors,
      headers: norm_opts[:headers]
    }

    %__MODULE__{
      virtual_channel: virtual_channel,
      resolver: resolver,
      adapter: adapter,
      resolver_target: norm_target,
      connect_opts: norm_opts
    }
  end

  defp resolve_credential(nil, @secure_scheme), do: default_ssl_option()
  defp resolve_credential(%GRPC.Credential{} = cred, _scheme), do: cred
  defp resolve_credential(nil, _scheme), do: nil
  defp resolve_credential(other, _scheme), do: other

  defp validate_adapter_opts!(opts) when is_list(opts), do: :ok

  defp validate_adapter_opts!(_),
    do: raise(ArgumentError, ":adapter_opts must be a keyword list if present")

  defp build_compressor_list(compressor, accepted) when is_list(accepted) do
    [compressor | accepted]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  defp choose_lb_mod(config, lb_policy_opt) do
    lb_policy =
      cond do
        is_map(config) and Map.has_key?(config, :load_balancing_policy) ->
          config.load_balancing_policy

        lb_policy_opt ->
          lb_policy_opt

        true ->
          nil
      end

    choose_lb(lb_policy)
  end

  defp build_real_channels(addresses, %Channel{} = virtual_channel, norm_opts, adapter) do
    Map.new(addresses, fn %{port: port, address: host} ->
      case connect_real_channel(virtual_channel, host, port, norm_opts, adapter) do
        {:ok, ch} ->
          {build_address_key(host, port), {:connected, ch}}

        {:error, reason} ->
          {build_address_key(host, port), {:failed, reason}}
      end
    end)
  end

  defp build_address_key(host, port) do
    case host do
      {:local, _} ->
        "#{inspect(host)}:#{port}"

      _ ->
        "#{host}:#{port}"
    end
  end

  defp normalize_target_and_opts(target, opts) do
    uri = URI.parse(target)

    cond do
      uri.scheme == @secure_scheme and uri.host ->
        opts = Keyword.put_new_lazy(opts, :cred, &default_ssl_option/0)
        {"ipv4:#{uri.host}:#{uri.port}", opts, @secure_scheme}

      uri.scheme == @insecure_scheme and uri.host ->
        if opts[:cred],
          do: raise(ArgumentError, "invalid option for insecure (http) address: :cred")

        {"ipv4:#{uri.host}:#{uri.port}", opts, @insecure_scheme}

      # Compatibility mode: host:port or unix:path
      uri.scheme in [nil, ""] ->
        scheme = if opts[:cred], do: @secure_scheme, else: @insecure_scheme

        case String.split(target, ":") do
          [host, port] ->
            {"ipv4:#{host}:#{port}", opts, scheme}

          [path] ->
            {"unix://#{path}", opts, "unix"}
        end

      # Anything else (dns://, unix://, etc.) handled by resolver
      true ->
        {target, opts, if(opts[:cred], do: @secure_scheme, else: @insecure_scheme)}
    end
  end

  defp choose_lb(:round_robin), do: GRPC.Client.LoadBalancing.RoundRobin
  defp choose_lb(_), do: GRPC.Client.LoadBalancing.PickFirst

  defp connect_real_channel(%Channel{} = vc, host, port, opts, adapter) do
    %Channel{vc | host: host, port: port}
    |> adapter.connect(opts[:adapter_opts])
  end

  defp init_interceptors(interceptors) do
    Enum.map(interceptors, fn
      {interceptor, opts} -> {interceptor, interceptor.init(opts)}
      interceptor -> {interceptor, interceptor.init([])}
    end)
  end

  if {:module, CAStore} == Code.ensure_loaded(CAStore) do
    defp default_ssl_option do
      %GRPC.Credential{
        ssl: [
          verify: :verify_peer,
          depth: 99,
          cacertfile: CAStore.file_path()
        ]
      }
    end
  else
    defp default_ssl_option do
      raise """
      no GRPC credentials provided. Please either:

      - Pass the `:cred` option to `GRPC.Stub.connect/2,3`
      - Add `:castore` to your list of dependencies in `mix.exs`
      """
    end
  end
end

defmodule GRPC.Client.Connection do
  @moduledoc """
  Connection manager for gRPC client channels, with optional **load balancing**
  and **name resolution** support.

  A `Conn` process manages one or more underlying gRPC connections
  (`GRPC.Channel` structs) and exposes a **virtual channel** to be used by
  client stubs. The orchestration process runs as a `GenServer` registered
  globally (via `:global`), so only one orchestrator exists **per connection**
  in a BEAM node.

  ## Overview

  * `connect/2` – establishes a client connection (single or multi-channel).
  * `pick/2` – chooses a channel according to the active load-balancing policy.
  * `disconnect/1` – gracefully closes a connection and frees resources.

  Under the hood:

  * The target string is resolved using a [Resolver](GRPC.Client.Resolver).
  * Depending on the target and service config, a load-balancing module is chosen
    (e.g. `PickFirst`, `RoundRobin`).
  * The orchestrator periodically refreshes the LB decision to adapt to changes.

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

  ## Notes

    * The orchestrator refreshes the LB pick every 15 seconds.
  """
  use GenServer
  alias GRPC.Channel

  require Logger

  @insecure_scheme "http"
  @secure_scheme "https"
  @refresh_interval 15_000

  @type t :: %__MODULE__{
          virtual_channel: Channel.t(),
          real_channels: %{String.t() => Channel.t()},
          lb_mod: module() | nil,
          lb_state: term() | nil,
          resolver: module() | nil,
          adapter: module()
        }

  defstruct virtual_channel: nil,
            real_channels: %{},
            lb_mod: nil,
            lb_state: nil,
            resolver: nil,
            adapter: GRPC.Client.Adapters.Gun

  def child_spec(initial_state) do
    %{
      id: {__MODULE__, initial_state.virtual_channel.ref},
      start:
        {GenServer, :start_link,
         [__MODULE__, initial_state, [name: via(initial_state.virtual_channel.ref)]]},
      restart: :transient,
      type: :worker,
      shutdown: 5000
    }
  end

  @impl GenServer
  def init(%__MODULE__{} = state) do
    Process.flag(:trap_exit, true)

    # only now persist the chosen channel (which should already have adapter_payload
    # because build_initial_state connected real channels and set virtual_channel)
    :persistent_term.put(
      {__MODULE__, :lb_state, state.virtual_channel.ref},
      state.virtual_channel
    )

    Process.send_after(self(), :refresh, @refresh_interval)
    {:ok, state}
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

  Returns:

    * `{:ok, channel}` – a `GRPC.Channel` usable with stubs
    * `{:error, reason}` – if connection fails

  ## Examples

      iex> {:ok, ch} = GRPC.Client.Connection.connect("127.0.0.1:50051")
      iex> Grpc.Testing.TestService.Stub.empty_call(ch, %{})
  """
  @spec connect(String.t(), keyword()) :: {:ok, Channel.t()} | {:error, any()}
  def connect(target, opts \\ []) do
    supervisor_pid = Process.whereis(GRPC.Client.Supervisor)

    if is_nil(supervisor_pid) or !Process.alive?(supervisor_pid) do
      raise """
      GRPC.Client.Supervisor is not running. Please ensure it is started as part of your
      application's supervision tree:

          children = [
            {GRPC.Client.Supervisor, []}
          ]

          opts = [strategy: :one_for_one, name: MyApp.Supervisor]
          Supervisor.start_link(children, opts)

      You can also start it manually in scripts or test environments:

          {:ok, _pid} = DynamicSupervisor.start_link(strategy: :one_for_one, name: GRPC.Client.Supervisor)
      """
    end

    ref = make_ref()

    case build_initial_state(target, Keyword.merge(opts, ref: ref)) do
      {:ok, initial_state} ->
        ch = initial_state.virtual_channel

        case DynamicSupervisor.start_child(GRPC.Client.Supervisor, child_spec(initial_state)) do
          {:ok, _pid} ->
            {:ok, ch}

          {:error, {:already_started, _pid}} ->
            # race: someone else started it first, ask the running process for its current channel
            case pick_channel(opts) do
              {:ok, %Channel{} = channel} ->
                {:ok, channel}

              _ ->
                {:error, :no_connection}
            end

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
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
  @spec disconnect(Channel.t()) :: {:ok, Channel.t()} | {:error, any()}
  def disconnect(%Channel{ref: ref} = channel) do
    GenServer.call(via(ref), {:disconnect, channel})
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
  @spec pick_channel(Channel.t(), keyword()) :: {:ok, Channel.t()} | {:error, term()}
  def pick_channel(%Channel{ref: ref} = _channel, _opts \\ []) do
    case :persistent_term.get({__MODULE__, :lb_state, ref}, nil) do
      nil ->
        {:error, :no_connection}

      %Channel{} = channel ->
        {:ok, channel}
    end
  end

  @impl GenServer
  def handle_call({:disconnect, %Channel{adapter: adapter} = channel}, _from, state) do
    resp = {:ok, %Channel{channel | adapter_payload: %{conn_pid: nil}}}

    if Map.has_key?(state, :real_channels) do
      Enum.map(state.real_channels, fn {_key, {:ok, ch}} ->
        adapter.disconnect(ch)
      end)

      keys_to_delete = [:real_channels, :virtual_channel]
      new_state = Map.drop(state, keys_to_delete)

      {:reply, resp, new_state, {:continue, :stop}}
    else
      {:reply, resp, state, {:continue, :stop}}
    end
  end

  @impl GenServer
  def handle_info(
        :refresh,
        %{lb_mod: lb_mod, lb_state: lb_state, real_channels: channels, virtual_channel: vc} =
          state
      ) do
    {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

    channel_key = "#{prefer_host}:#{prefer_port}"

    case Map.get(channels, channel_key) do
      nil ->
        Logger.warning("LB picked #{channel_key}, but no channel found in pool")

        Process.send_after(self(), :refresh, @refresh_interval)
        {:noreply, %{state | lb_state: new_lb_state}}

      {:ok, %Channel{} = picked_channel} ->
        :persistent_term.put({__MODULE__, :lb_state, vc.ref}, picked_channel)

        Process.send_after(self(), :refresh, @refresh_interval)

        {:noreply, %{state | lb_state: new_lb_state, virtual_channel: picked_channel}}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    Logger.warning(
      "#{inspect(__MODULE__)} received :DOWN from #{inspect(pid)} with reason: #{inspect(reason)}"
    )

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.warning("#{inspect(__MODULE__)} received unexpected message: #{inspect(msg)}")

    {:noreply, state}
  end

  @impl GenServer
  def handle_continue(:stop, state) do
    Logger.info("#{inspect(__MODULE__)} stopping as requested")
    {:stop, :normal, state}
  end

  @impl GenServer
  def terminate(_reason, _state), do: :ok

  defp via(ref) do
    {:global, {__MODULE__, ref}}
  end

  defp build_initial_state(target, opts) do
    opts =
      Keyword.validate!(opts,
        cred: nil,
        ref: nil,
        adapter: GRPC.Client.Adapters.Gun,
        adapter_opts: [],
        interceptors: [],
        codec: GRPC.Codec.Proto,
        compressor: nil,
        accepted_compressors: [],
        headers: []
      )

    resolver = Keyword.get(opts, :resolver, GRPC.Client.Resolver)
    adapter = Keyword.get(opts, :adapter, GRPC.Client.Adapters.Gun)
    lb_policy_opt = Keyword.get(opts, :lb_policy)

    {norm_target, norm_opts, scheme} = normalize_target_and_opts(target, opts)
    cred = resolve_credential(norm_opts[:cred], scheme)
    interceptors = init_interceptors(norm_opts[:interceptors])

    accepted_compressors =
      build_compressor_list(norm_opts[:compressor], norm_opts[:accepted_compressors])

    validate_adapter_opts!(opts[:adapter_opts])

    virtual_channel = %Channel{
      scheme: scheme,
      cred: cred,
      ref: opts[:ref],
      adapter: adapter,
      interceptors: interceptors,
      codec: norm_opts[:codec],
      compressor: norm_opts[:compressor],
      accepted_compressors: accepted_compressors,
      headers: norm_opts[:headers]
    }

    base_state = %__MODULE__{
      virtual_channel: virtual_channel,
      resolver: resolver,
      adapter: adapter
    }

    case resolver.resolve(norm_target) do
      {:ok, %{addresses: addresses, service_config: config}} ->
        build_balanced_state(base_state, addresses, config, lb_policy_opt, norm_opts, adapter)

      {:error, _reason} ->
        build_direct_state(base_state, norm_target, norm_opts, adapter)
    end
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

  defp build_balanced_state(base_state, addresses, config, lb_policy_opt, norm_opts, adapter) do
    lb_policy =
      cond do
        is_map(config) and Map.has_key?(config, :load_balancing_policy) ->
          config.load_balancing_policy

        lb_policy_opt ->
          lb_policy_opt

        true ->
          nil
      end

    lb_mod = choose_lb(lb_policy)

    case lb_mod.init(addresses: addresses) do
      {:ok, lb_state} ->
        {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

        real_channels =
          build_real_channels(addresses, base_state.virtual_channel, norm_opts, adapter)

        key = build_address_key(prefer_host, prefer_port)

        with {:ok, ch} <- Map.get(real_channels, key, {:error, :no_channel}) do
          {:ok,
           %__MODULE__{
             base_state
             | lb_mod: lb_mod,
               lb_state: new_lb_state,
               virtual_channel: ch,
               real_channels: real_channels
           }}
        else
          {:error, reason} -> {:error, reason}
        end

      {:error, :no_addresses} ->
        {:error, :no_addresses}
    end
  end

  defp build_direct_state(base_state, norm_target, norm_opts, adapter) do
    {host, port} = split_host_port(norm_target)
    vc = base_state.virtual_channel

    case connect_real_channel(vc, host, port, norm_opts, adapter) do
      {:ok, ch} ->
        {:ok,
         %__MODULE__{
           base_state
           | virtual_channel: ch,
             real_channels: %{"#{host}:#{port}" => ch}
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_real_channels(addresses, virtual_channel, norm_opts, adapter) do
    Map.new(addresses, fn %{port: port, address: host} ->
      case connect_real_channel(
             %Channel{virtual_channel | host: host, port: port},
             host,
             port,
             norm_opts,
             adapter
           ) do
        {:ok, ch} ->
          {build_address_key(host, port), {:ok, ch}}

        {:error, reason} ->
          {build_address_key(host, port), {:error, reason}}
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

  defp connect_real_channel(%Channel{scheme: "unix"} = vc, path, port, opts, adapter) do
    %Channel{vc | host: path, port: port}
    |> adapter.connect(opts[:adapter_opts])
  end

  defp connect_real_channel(vc, host, port, opts, adapter) do
    %Channel{vc | host: host, port: port}
    |> adapter.connect(opts[:adapter_opts])
  end

  defp split_host_port(target) do
    case String.split(target, ":", trim: true) do
      [h, p] -> {h, String.to_integer(p)}
      [h] -> {h, default_port()}
    end
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
          cacert_file: CAStore.file_path()
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

  defp default_port, do: 50051
end

defmodule GRPC.Client.Connection do
  @moduledoc """
  Connection manager for gRPC client channels, with optional load balancing
  and name resolution support.

  A `Conn` process manages one or more underlying gRPC connections
  (`GRPC.Channel` structs) and exposes a virtual channel to be used by
  client stubs. The orchestration process runs as a `GenServer` registered
  globally (via `:global`), so only one orchestrator exists per connection
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
  @default_resolve_interval 30_000
  @default_max_resolve_interval 300_000
  @default_min_resolve_interval 5_000

  @type t :: %__MODULE__{
          virtual_channel: Channel.t(),
          real_channels: %{String.t() => {:ok, Channel.t()} | {:error, any()}},
          lb_mod: module() | nil,
          lb_state: term() | nil,
          resolver: module() | nil,
          adapter: module(),
          target: String.t() | nil,
          connect_opts: keyword(),
          dns_resolver: pid() | nil
        }

  defstruct virtual_channel: nil,
            real_channels: %{},
            lb_mod: nil,
            lb_state: nil,
            resolver: nil,
            adapter: GRPC.Client.Adapters.Gun,
            target: nil,
            connect_opts: [],
            dns_resolver: nil

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

    # Only start periodic re-resolution for DNS targets — static targets
    # (ipv4:, ipv6:, unix:) always resolve to the same addresses.
    state =
      if state.resolver && state.target && dns_target?(state.target) do
        {:ok, pid} =
          GRPC.Client.DnsResolver.start_link(
            connection_pid: self(),
            resolver: state.resolver,
            target: state.target,
            resolve_interval: state.connect_opts[:resolve_interval] || @default_resolve_interval,
            max_resolve_interval: state.connect_opts[:max_resolve_interval] || @default_max_resolve_interval,
            min_resolve_interval: state.connect_opts[:min_resolve_interval] || @default_min_resolve_interval
          )

        %{state | dns_resolver: pid}
      else
        state
      end

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
    case build_initial_state(target, opts) do
      {:ok, initial_state} ->
        ch = initial_state.virtual_channel

        case DynamicSupervisor.start_child(GRPC.Client.Supervisor, child_spec(initial_state)) do
          {:ok, _pid} ->
            {:ok, ch}

          {:error, {:already_started, _pid}} ->
            case pick_channel(ch, opts) do
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

  @doc """
  Triggers an immediate DNS re-resolution, subject to rate limiting.

  Intended for use by health checks or heartbeat mechanisms that detect
  a backend has gone away and want to force a fresh DNS lookup.
  """
  @spec resolve_now(Channel.t()) :: :ok | {:error, :no_connection}
  def resolve_now(%Channel{ref: ref}) do
    case :global.whereis_name({__MODULE__, ref}) do
      :undefined -> {:error, :no_connection}
      pid -> GenServer.call(pid, :resolve_now)
    end
  end

  @impl GenServer
  def handle_call(:resolve_now, _from, %{dns_resolver: pid} = state) when is_pid(pid) do
    send(pid, :resolve_now)
    {:reply, :ok, state}
  end

  def handle_call(:resolve_now, _from, state) do
    {:reply, {:error, :no_dns_resolver}, state}
  end

  def handle_call({:disconnect, %Channel{adapter: adapter} = channel}, _from, state) do
    resp = {:ok, %Channel{channel | adapter_payload: %{conn_pid: nil}}}
    :persistent_term.erase({__MODULE__, :lb_state, channel.ref})

    if Map.has_key?(state, :real_channels) do
      Enum.map(state.real_channels, fn
        {_key, {:ok, ch}} ->
          do_disconnect(adapter, ch)

        _ ->
          :ok
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
      )
      when not is_nil(lb_mod) do
    {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

    channel_key = build_address_key(prefer_host, prefer_port)

    case Map.get(channels, channel_key) do
      {:ok, %Channel{} = picked_channel} ->
        :persistent_term.put({__MODULE__, :lb_state, vc.ref}, picked_channel)

        Process.send_after(self(), :refresh, @refresh_interval)
        {:noreply, %{state | lb_state: new_lb_state, virtual_channel: picked_channel}}

      _nil_or_error ->
        # LB picked a channel that is missing or in {:error, _} state.
        # Don't update persistent_term — keep serving from the current
        # virtual_channel until re-resolution provides healthy backends.
        Logger.warning("LB picked #{channel_key}, but channel is unavailable")

        Process.send_after(self(), :refresh, @refresh_interval)
        {:noreply, %{state | lb_state: new_lb_state}}
    end
  end

  def handle_info(:refresh, state), do: {:noreply, state}

  # Result from the dedicated DnsResolver process
  def handle_info({:dns_result, result}, state) do
    state = handle_resolve_result(result, state)
    {:noreply, state}
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
    Logger.debug("#{inspect(__MODULE__)} stopping as requested")
    {:stop, :normal, state}
  end

  @impl GenServer
  def terminate(_reason, %{virtual_channel: %{ref: ref}}) do
    :persistent_term.erase({__MODULE__, :lb_state, ref})
  rescue
    _ -> :ok
  end

  def terminate(_reason, _state), do: :ok

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

    # Disconnect removed channels
    real_channels =
      Enum.reduce(MapSet.to_list(removed), state.real_channels, fn key, channels ->
        case Map.get(channels, key) do
          {:ok, ch} -> do_disconnect(adapter, ch)
          _ -> :ok
        end

        Map.delete(channels, key)
      end)

    # Connect new channels, retry failed ones, and reconnect dead adapter PIDs
    real_channels =
      Enum.reduce(new_addresses, real_channels, fn %{address: host, port: port}, channels ->
        key = build_address_key(host, port)
        existing = Map.get(channels, key)

        should_connect =
          MapSet.member?(added, key) or
            match?({:error, _}, existing) or
            not channel_alive?(existing)

        if should_connect do
          # Disconnect the old channel if it exists but is dead
          case existing do
            {:ok, ch} -> do_disconnect(adapter, ch)
            _ -> :ok
          end

          case connect_real_channel(state.virtual_channel, host, port, opts, adapter) do
            {:ok, ch} -> Map.put(channels, key, {:ok, ch})
            {:error, reason} -> Map.put(channels, key, {:error, reason})
          end
        else
          channels
        end
      end)

    # Re-init load balancer with full updated address list.
    #
    # NOTE: We guard persistent_term writes to only happen when the picked
    # channel actually changes. persistent_term updates trigger a global GC
    # pass across all BEAM processes (see erlang.org/doc/apps/erts/persistent_term).
    # With periodic re-resolution this function runs every 30s+ per connection,
    # and on no-change cycles we must avoid redundant writes. A future
    # improvement would be migrating to ETS with read_concurrency: true,
    # which has no global GC cost on writes.
    if state.lb_mod do
      case state.lb_mod.init(addresses: new_addresses) do
        {:ok, new_lb_state} ->
          {:ok, {host, port}, picked_lb_state} = state.lb_mod.pick(new_lb_state)
          key = build_address_key(host, port)

          case Map.get(real_channels, key) do
            {:ok, picked_channel} ->
              maybe_update_persistent_term(state.virtual_channel, picked_channel)

              %{
                state
                | real_channels: real_channels,
                  lb_state: picked_lb_state,
                  virtual_channel: picked_channel
              }

            _ ->
              # LB picked a channel that failed to connect. Fall back to any
              # healthy channel so persistent_term doesn't hold a dead ref.
              fallback_to_healthy_channel(state, real_channels, picked_lb_state)
          end

        {:error, _} ->
          fallback_to_healthy_channel(state, real_channels, state.lb_state)
      end
    else
      fallback_to_healthy_channel(state, real_channels, state.lb_state)
    end
  end

  defp fallback_to_healthy_channel(state, real_channels, lb_state) do
    ref = state.virtual_channel.ref

    case Enum.find_value(real_channels, fn {_k, v} -> match?({:ok, _}, v) && v end) do
      {:ok, healthy_channel} ->
        maybe_update_persistent_term(state.virtual_channel, healthy_channel)

        %{
          state
          | real_channels: real_channels,
            lb_state: lb_state,
            virtual_channel: healthy_channel
        }

      nil ->
        Logger.warning("No healthy channels available after re-resolution")
        :persistent_term.erase({__MODULE__, :lb_state, ref})
        %{state | real_channels: real_channels, lb_state: lb_state}
    end
  end

  # Only write to persistent_term when the channel actually changed.
  # persistent_term updates trigger a global GC pass, so we skip
  # redundant writes on no-change re-resolution cycles.
  defp maybe_update_persistent_term(current_channel, new_channel) do
    if current_channel != new_channel do
      :persistent_term.put(
        {__MODULE__, :lb_state, new_channel.ref},
        new_channel
      )
    end
  end

  defp channel_alive?({:ok, %{adapter_payload: %{conn_pid: pid}}}) when is_pid(pid) do
    Process.alive?(pid)
  end

  defp channel_alive?({:ok, _}), do: true
  defp channel_alive?(_), do: false

  defp dns_target?(target) do
    URI.parse(target).scheme == "dns"
  end

  defp via(ref) do
    {:global, {__MODULE__, ref}}
  end

  defp do_disconnect(adapter, channel) do
    adapter.disconnect(channel)
  rescue
    _ ->
      :ok
  catch
    _type, _value ->
      :ok
  end

  defp build_initial_state(target, opts) do
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
        resolver: GRPC.Client.Resolver,
        resolve_interval: @default_resolve_interval,
        max_resolve_interval: @default_max_resolve_interval,
        min_resolve_interval: @default_min_resolve_interval
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
      ref: opts[:name],
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
      adapter: adapter,
      target: norm_target,
      connect_opts: norm_opts
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

  defp build_balanced_state(
         %__MODULE__{} = base_state,
         addresses,
         config,
         lb_policy_opt,
         norm_opts,
         adapter
       ) do
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

  defp build_direct_state(%__MODULE__{} = base_state, norm_target, norm_opts, adapter) do
    {host, port} = split_host_port(norm_target)
    vc = base_state.virtual_channel

    case connect_real_channel(vc, host, port, norm_opts, adapter) do
      {:ok, ch} ->
        {:ok,
         %__MODULE__{
           base_state
           | virtual_channel: ch,
             real_channels: %{"#{host}:#{port}" => {:ok, ch}}
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_real_channels(addresses, %Channel{} = virtual_channel, norm_opts, adapter) do
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

  defp connect_real_channel(%Channel{} = vc, host, port, opts, adapter) do
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

  defp default_port, do: 50051
end

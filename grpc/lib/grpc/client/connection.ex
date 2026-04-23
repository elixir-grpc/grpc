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
  * Each call to `pick/2` dispatches to the LB module, which selects a channel
    per request. DNS re-resolution reconciles the LB's channel list in place.

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
  alias GRPC.Client.LoadBalancing.Registry

  require Logger

  @insecure_scheme "http"
  @secure_scheme "https"
  @default_resolve_interval 30_000
  @default_max_resolve_interval 300_000
  @default_min_resolve_interval 5_000

  @type t :: %__MODULE__{
          virtual_channel: Channel.t(),
          real_channels: %{String.t() => {:connected, Channel.t()} | {:failed, any()}},
          lb_mod: module() | nil,
          lb_state: term() | nil,
          resolver: module() | nil,
          adapter: module(),
          resolver_target: String.t() | nil,
          connect_opts: keyword(),
          resolver_state: term() | nil
        }

  defstruct virtual_channel: nil,
            real_channels: %{},
            lb_mod: nil,
            lb_state: nil,
            resolver: nil,
            adapter: GRPC.Client.Adapters.Gun,
            resolver_target: nil,
            connect_opts: [],
            resolver_state: nil

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

    Registry.put(state.virtual_channel.ref, {state.lb_mod, state.lb_state})

    state =
      if function_exported?(state.resolver, :init, 2) do
        {:ok, resolver_state} =
          state.resolver.init(state.resolver_target,
            connection_pid: self(),
            connect_opts: state.connect_opts
          )

        %{state | resolver_state: resolver_state}
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
    * `:resolve_interval` – DNS re-resolution interval in ms (default: 30000)
    * `:max_resolve_interval` – backoff cap in ms (default: 300000)
    * `:min_resolve_interval` – rate-limit floor in ms (default: 5000)

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
    case Registry.lookup(ref) do
      {:ok, {lb_mod, lb_state}} when not is_nil(lb_mod) ->
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
  @spec resolve_now(Channel.t()) :: :ok
  def resolve_now(%Channel{ref: ref}) do
    GenServer.cast(via(ref), :resolve_now)
  end

  @impl GenServer
  def handle_cast(:resolve_now, %{resolver: resolver, resolver_state: rs} = state)
      when not is_nil(rs) do
    {:ok, new_rs} = resolver.update(rs, :resolve_now)
    {:noreply, %{state | resolver_state: new_rs}}
  end

  def handle_cast(:resolve_now, state), do: {:noreply, state}

  @impl GenServer
  def handle_call({:disconnect, %Channel{adapter: adapter} = channel}, _from, state) do
    if state.resolver_state && function_exported?(state.resolver, :shutdown, 1) do
      state.resolver.shutdown(state.resolver_state)
    end

    shutdown_lb(state.lb_mod, state.lb_state)

    resp = {:ok, %Channel{channel | adapter_payload: %{conn_pid: nil}}}
    Registry.delete(channel.ref)

    if Map.has_key?(state, :real_channels) do
      Enum.map(state.real_channels, fn
        {_key, {:connected, ch}} ->
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
  def handle_info({:resolver_update, result}, state) do
    state = handle_resolve_result(result, state)
    {:noreply, state}
  end

  def handle_info({:EXIT, _pid, reason}, %{resolver: resolver, resolver_state: rs} = state)
      when not is_nil(rs) and reason != :normal do
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
  def terminate(_reason, %{virtual_channel: %{ref: ref}} = state) do
    shutdown_lb(Map.get(state, :lb_mod), Map.get(state, :lb_state))
    Registry.delete(ref)
  rescue
    _ -> :ok
  end

  def terminate(_reason, _state), do: :ok

  defp shutdown_lb(lb_mod, lb_state)
       when not is_nil(lb_mod) and not is_nil(lb_state) do
    if function_exported?(lb_mod, :shutdown, 1) do
      lb_mod.shutdown(lb_state)
    end
  rescue
    _ -> :ok
  end

  defp shutdown_lb(_lb_mod, _lb_state), do: :ok

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

    rebalance_after_reconcile(new_addresses, real_channels, state)
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

  defp rebalance_after_reconcile(_new_addresses, real_channels, state) do
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
    if lb_state != nil and function_exported?(lb_mod, :update, 2) do
      lb_mod.update(lb_state, new_channels)
    else
      lb_mod.init(channels: new_channels)
    end
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
      resolver_target: norm_target,
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

    real_channels =
      build_real_channels(addresses, base_state.virtual_channel, norm_opts, adapter)

    connected = connected_channels(real_channels)

    case lb_mod.init(channels: connected) do
      {:ok, lb_state} ->
        {:ok, %Channel{} = picked, _} = lb_mod.pick(lb_state)

        {:ok,
         %__MODULE__{
           base_state
           | lb_mod: lb_mod,
             lb_state: lb_state,
             virtual_channel: picked,
             real_channels: real_channels
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_direct_state(%__MODULE__{} = base_state, norm_target, norm_opts, adapter) do
    {host, port} = split_host_port(norm_target)
    vc = base_state.virtual_channel
    lb_mod = GRPC.Client.LoadBalancing.PickFirst

    with {:ok, ch} <- connect_real_channel(vc, host, port, norm_opts, adapter),
         {:ok, lb_state} <- lb_mod.init(channels: [ch]) do
      {:ok,
       %__MODULE__{
         base_state
         | virtual_channel: ch,
           real_channels: %{build_address_key(host, port) => {:connected, ch}},
           lb_mod: lb_mod,
           lb_state: lb_state
       }}
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

defmodule GRPC.Client.Conn do
  @moduledoc """
  Connection manager for gRPC client with optional load balancing support.

  This process is registered globally under its module name (`__MODULE__`),
  so only one connection orchestrator exists per BEAM node.
  """
  use GenServer
  alias GRPC.Channel

  @insecure_scheme "http"
  @secure_scheme "https"

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

  @doc """
  Connect to a server or set of servers.

  If a load balancing policy is configured (via resolver or `:lb_policy`),
  the connection orchestrator will manage multiple channels internally.
  """
  @spec connect(String.t(), keyword()) :: {:ok, Channel.t()} | {:error, any()}
  def connect(target, opts \\ []) do
    case GenServer.whereis(__MODULE__) do
      nil ->
        initial_state = build_initial_state(target, opts)

        case GenServer.start_link(__MODULE__, initial_state, name: __MODULE__) do
          {:ok, _pid} ->
            {:ok, initial_state.virtual_channel}

          {:error, {:already_started, pid}} ->
            # Race condition between whereis and start_link
            {:ok, GenServer.call(pid, :get_channel)}

          {:error, reason} ->
            {:error, reason}
        end

      pid ->
        {:ok, GenServer.call(pid, :get_channel)}
    end
  end

  @spec disconnect(Channel.t()) :: {:ok, Channel.t()} | {:error, any()}
  def disconnect(%Channel{} = channel) do
    GenServer.call(__MODULE__, {:disconnect, channel})
  end

  @doc """
  Pick a connection channel according to the current LB policy.
  """
  @spec pick(keyword()) :: {:ok, Channel.t()} | {:error, term()}
  def pick(opts \\ []) do
    GenServer.call(__MODULE__, {:pick, opts})
  end

  @impl true
  def init(%__MODULE__{} = state), do: {:ok, state}

  @impl true
  def handle_call(:get_channel, _from, %{virtual_channel: ch} = state) do
    {:reply, ch, state}
  end

  @impl true
  def handle_call({:disconnect, %Channel{adapter: adapter} = channel}, _from, state) do
    resp = {:ok, %Channel{channel | adapter_payload: %{conn_pid: nil}}}

    if Map.has_key?(state, :real_channels) do
      Enum.map(state.real_channels, fn {_key, ch} ->
        adapter.disconnect(ch)
      end)

      keys_to_delete = [:real_channels, :virtual_channel]

      new_state =
        Enum.reduce(keys_to_delete, state, fn key, acc ->
          if Map.has_key?(acc, key), do: Map.delete(acc, key), else: acc
        end)

      {:reply, resp, new_state}
    else
      {:reply, resp, state}
    end
  end

  @impl true
  def handle_call(
        {:pick, _opts},
        _from,
        %{lb_mod: lb_mod, lb_state: lb_state, real_channels: channels} = state
      ) do
    {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

    channel_key = "#{inspect(prefer_host)}:#{prefer_port}"
    channel = Map.get(channels, channel_key)

    {:reply, {:ok, channel}, %{state | lb_state: new_lb_state}}
  end

  defp build_initial_state(target, opts) do
    opts =
      Keyword.validate!(opts,
        cred: nil,
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

    cred =
      case norm_opts[:cred] do
        nil when scheme == @secure_scheme ->
          default_ssl_option()

        %GRPC.Credential{} = c ->
          c

        nil ->
          nil

        other ->
          other
      end

    intialized_interceptors = init_interceptors(norm_opts[:interceptors])
    codec = norm_opts[:codec]
    compressor = norm_opts[:compressor]
    headers = norm_opts[:headers]

    accepted_compressors =
      [compressor | norm_opts[:accepted_compressors]]
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    adapter_opts = opts[:adapter_opts]

    unless is_list(adapter_opts),
      do: raise(ArgumentError, ":adapter_opts must be a keyword list if present")

    virtual_channel = %Channel{
      scheme: scheme,
      cred: cred,
      adapter: adapter,
      interceptors: intialized_interceptors,
      codec: codec,
      compressor: compressor,
      accepted_compressors: accepted_compressors,
      headers: headers
    }

    base_state = %__MODULE__{
      virtual_channel: virtual_channel,
      resolver: resolver,
      adapter: adapter
    }

    case resolver.resolve(norm_target) do
      {:ok, %{addresses: addresses, service_config: config}} ->
        lb_policy =
          cond do
            is_map(config) && Map.has_key?(config, :load_balancing_policy) ->
              config.load_balancing_policy

            lb_policy_opt ->
              lb_policy_opt

            true ->
              nil
          end

        lb_mod = choose_lb(lb_policy)
        {:ok, lb_state} = lb_mod.init(addresses: addresses)

        {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

        real_channels =
          Enum.into(addresses, %{}, fn %{port: port, address: host} ->
            case connect_real_channel(
                   %Channel{virtual_channel | host: host, port: port},
                   host,
                   port,
                   norm_opts,
                   adapter
                 ) do
              {:ok, ch} ->
                {"#{inspect(host)}:#{port}", ch}

              {:error, :timeout} ->
                {"#{host}:#{port}", %Channel{virtual_channel | host: host, port: port}}

              {:error, reason} ->
                raise "Failed to connect to #{inspect(host)}:#{port} - #{inspect(reason)}"
            end
          end)

        virtual_channel =
          Map.get(real_channels, "#{inspect(prefer_host)}:#{prefer_port}")

        %__MODULE__{
          base_state
          | lb_mod: lb_mod,
            lb_state: new_lb_state,
            virtual_channel: virtual_channel,
            real_channels: real_channels
        }

      {:error, _reason} ->
        {host, port} = split_host_port(norm_target)
        {:ok, ch} = connect_real_channel(virtual_channel, host, port, norm_opts, adapter)

        %__MODULE__{
          base_state
          | virtual_channel: ch,
            real_channels: %{"#{host}:#{port}" => ch}
        }
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
    case String.split(target, ":") do
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

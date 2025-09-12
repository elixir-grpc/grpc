defmodule GRPC.Client.Conn do
  @moduledoc """
  Connection manager for gRPC client with optional load balancing support.
  """
  use GenServer
  alias GRPC.Channel

  @default_timeout 10_000
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
  Connect to a server or set of servers. If `:lb_policy` is provided, enables
  load balancing.
  """
  @spec connect(String.t(), keyword()) :: {:ok, pid()} | {:error, any()}
  def connect(target, opts \\ []) do
    initial_state = build_initial_state(target, opts)

    case GenServer.start_link(__MODULE__, initial_state) do
      {:ok, pid} ->
        initial_state.virtual_channel

      {:error, {:already_started, pid}} ->
        {:error, nil}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Pick a connection channel.
  """
  @spec pick(keyword()) :: any()
  def pick(opts \\ []) do
    GenServer.call(__MODULE__, {:pick, opts})
  end

  @impl true
  def init(%__MODULE__{} = state), do: {:ok, state}

  @impl true
  def handle_call(
        {:pick, _opts},
        _from,
        %{lb_mod: lb_mod, lb_state: lb_state, real_channels: channels} = state
      ) do
    # Pick a real channel using load balancing policy if available
    {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)

    channel_key = "#{prefer_host}:#{prefer_port}"
    channel = Map.get(channels, channel_key)

    {:reply, {:ok, channel}, state}
  end

  defp build_initial_state(target, opts) do
    resolver = Keyword.get(opts, :resolver, GRPC.Client.Resolver)
    adapter = Keyword.get(opts, :adapter, GRPC.Client.Adapters.Gun)
    lb_policy_opt = Keyword.get(opts, :lb_policy)

    virtual_channel = %Channel{
      adapter: adapter,
      interceptors: init_interceptors(opts[:interceptors] || []),
      codec: opts[:codec] || GRPC.Codec.Proto,
      compressor: opts[:compressor],
      accepted_compressors: opts[:accepted_compressors] || [],
      headers: opts[:headers] || []
    }

    base_state = %__MODULE__{
      virtual_channel: virtual_channel,
      resolver: resolver,
      adapter: adapter
    }

    case resolver.resolve(target) do
      {:ok, %{addresses: addresses, service_config: config}} ->
        lb_mod = choose_lb(config.load_balancing_policy || lb_policy_opt)
        {:ok, lb_state} = lb_mod.init(addresses: addresses)

        {:ok, {prefer_host, prefer_port}, new_lb_state} = lb_mod.pick(lb_state)
        virtual_channel = %Channel{virtual_channel | host: prefer_host, port: prefer_port}

        real_channels =
          Enum.into(addresses, %{}, fn {host, port} ->
            {:ok, ch} = connect_real_channel(virtual_channel, host, port, opts, adapter)
            {"#{host}:#{port}", ch}
          end)

        %__MODULE__{
          base_state
          | lb_mod: lb_mod,
            lb_state: new_lb_state,
            virtual_channel: virtual_channel,
            real_channels: real_channels
        }

      {:error, _reason} ->
        {host, port} = split_host_port(target)
        {:ok, ch} = connect_real_channel(virtual_channel, host, port, opts, adapter)

        %__MODULE__{
          base_state
          | virtual_channel: ch,
            real_channels: %{"#{host}:#{port}" => ch}
        }
    end
  end

  defp choose_lb(:round_robin), do: GRPC.Client.LoadBalancing.RoundRobin
  defp choose_lb(_), do: GRPC.Client.LoadBalancing.PickFirst

  defp split_host_port(target) do
    case String.split(target, ":") do
      [h, p] -> {h, String.to_integer(p)}
      [h] -> {h, default_port()}
    end
  end

  defp connect_real_channel(virtual_channel, host, port, opts, adapter) do
    cred = opts[:cred]
    scheme = if cred, do: @secure_scheme, else: @insecure_scheme

    %Channel{
      virtual_channel
      | host: host,
        port: port,
        scheme: scheme,
        cred: cred
    }
    |> adapter.connect(opts[:adapter_opts] || [])
  end

  defp init_interceptors(interceptors) do
    Enum.map(interceptors, fn
      {interceptor, opts} -> {interceptor, interceptor.init(opts)}
      interceptor -> {interceptor, interceptor.init([])}
    end)
  end

  defp default_port, do: 50051
end

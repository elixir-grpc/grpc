defmodule GRPC.Server.Adapters.ThousandIsland.Supervisor do
  @moduledoc """
  Supervisor for ThousandIsland adapter.

  This supervisor manages the lifecycle of the ThousandIsland server and
  provides isolation from other adapters. It encapsulates all ThousandIsland-specific
  configuration and startup logic.

  ## Supervision Tree

  ```
  GRPC.Server.Supervisor
  └── ThousandIsland.Supervisor (this module)
      └── ThousandIsland (actual socket server)
          ├── Acceptor Pool
          ├── Connection Handlers
          └── Handler Processes
  ```

  ## Responsibilities

  - Configures ThousandIsland server with gRPC-specific settings
  - Manages HTTP/2 settings and transport options
  - Handles SSL/TLS configuration
  - Provides clean shutdown on termination
  """

  use Supervisor
  require Logger

  alias GRPC.Server.Adapters.ThousandIsland.Handler

  @default_num_acceptors 10
  @default_max_connections 16_384

  @doc """
  Starts the ThousandIsland supervisor.

  ## Options

    * `:endpoint` - The endpoint module (optional)
    * `:servers` - Map of service name => server modules
    * `:port` - The port to listen on
    * `:adapter_opts` - ThousandIsland-specific options (see below)
    * `:cred` - SSL credentials (optional, for HTTPS)

  ## Adapter Options

    * `:num_acceptors` - Number of acceptor processes (default: 10)
    * `:num_connections` - Maximum number of connections (default: 16384)
    * `:ip` - IP address to bind to (default: {0, 0, 0, 0})
    * `:transport_options` - Additional transport options
  """
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    endpoint = opts[:endpoint]
    servers = opts[:servers]
    port = opts[:port]

    server_opts = build_server_opts(endpoint, servers, port, opts)

    scheme = if cred_opts(opts), do: :https, else: :http
    server_name = server_names(endpoint, servers)

    Logger.info("Starting #{server_name} with ThousandIsland using #{scheme}://0.0.0.0:#{port}")

    children = [
      {Task.Supervisor, name: GRPC.Server.StreamTaskSupervisor},
      {ThousandIsland, server_opts}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp build_server_opts(endpoint, servers, port, opts) do
    adapter_opts = Keyword.get(opts, :adapter_opts, opts)

    num_acceptors = Keyword.get(adapter_opts, :num_acceptors, @default_num_acceptors)
    num_connections = Keyword.get(adapter_opts, :num_connections, @default_max_connections)

    transport_opts =
      adapter_opts
      |> Keyword.get(:transport_options, [])
      |> Keyword.put(:port, port)
      |> maybe_add_ip(adapter_opts)
      |> maybe_add_ssl(cred_opts(opts))
      # Optimize TCP buffers for gRPC performance (support up to 1MB messages)
      # 1MB buffer for large messages
      |> Keyword.put_new(:buffer, 1_048_576)
      # 1MB receive buffer
      |> Keyword.put_new(:recbuf, 1_048_576)
      # 1MB send buffer
      |> Keyword.put_new(:sndbuf, 1_048_576)
      # Disable Nagle's algorithm for low latency
      |> Keyword.put_new(:nodelay, true)

    # Configure HTTP/2 settings for larger frames (needed for large gRPC messages)
    local_settings = [
      # 1MB window size for large payloads
      initial_window_size: 1_048_576,
      # Keep default max frame size
      max_frame_size: 16_384
    ]

    handler_options = %{
      endpoint: endpoint,
      servers: servers,
      opts: [local_settings: local_settings]
    }

    Logger.debug("[ThousandIsland.Supervisor] Creating server configuration")

    [
      port: port,
      transport_module: transport_module(opts),
      transport_options: transport_opts,
      handler_module: Handler,
      handler_options: handler_options,
      num_acceptors: num_acceptors,
      num_connections: num_connections
    ]
  end

  defp maybe_add_ip(transport_opts, adapter_opts) do
    case Keyword.get(adapter_opts, :ip) do
      nil -> transport_opts
      ip -> Keyword.put(transport_opts, :ip, ip)
    end
  end

  defp maybe_add_ssl(transport_opts, nil), do: transport_opts

  defp maybe_add_ssl(transport_opts, cred_opts) do
    Keyword.merge(transport_opts, cred_opts.ssl)
  end

  defp transport_module(opts) do
    if cred_opts(opts) do
      ThousandIsland.Transports.SSL
    else
      ThousandIsland.Transports.TCP
    end
  end

  defp cred_opts(opts) do
    opts[:cred]
  end

  defp server_names(nil, servers) do
    Enum.map_join(servers, ",", fn {_k, s} -> inspect(s) end)
  end

  defp server_names(endpoint, _) do
    inspect(endpoint)
  end
end

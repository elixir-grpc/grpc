defmodule Grpc.Testing.WorkerService.Server do
  use GRPC.Server, service: Grpc.Testing.WorkerService.Service
  alias GRPC.Server

  alias Benchmark.ServerManager
  alias Benchmark.ClientManager

  require Logger

  def run_server(args_enum, stream) do
    Enum.reduce(args_enum, nil, fn args, server ->
      Logger.debug("Server got args:")
      Logger.debug(inspect(args))

      {server, status} =
        case args.argtype do
          {:setup, config} ->
            server = ServerManager.start_server(config)
            Logger.debug("Started server: #{inspect(server)}")

            {server, stats} = Benchmark.Server.get_stats(server)

            status =
              Grpc.Testing.ServerStatus.new(
                stats: stats,
                port: server.port,
                cores: server.cores
              )

            {server, status}

          {:mark, mark} ->
            {server, stats} = Benchmark.Server.get_stats(server, mark)

            status =
              Grpc.Testing.ServerStatus.new(cores: server.cores, port: server.port, stats: stats)

            {server, status}
        end

      Logger.debug("Server send reply #{inspect(status)}")
      Server.send_reply(stream, status)
      server
    end)
  end

  def run_client(args_enum, stream) do
    Enum.reduce(args_enum, nil, fn args, manager ->
      Logger.debug("Client got args:")
      Logger.debug(inspect(args))

      {status, manager} =
        case args.argtype do
          {:setup, client_config} ->
            manager = ClientManager.start_client(client_config)
            {Grpc.Testing.ClientStatus.new(), manager}

          {:mark, mark} ->
            stats = ClientManager.get_stats(manager, mark.reset)
            {Grpc.Testing.ClientStatus.new(stats: stats), manager}
        end

      Logger.debug("Client send reply #{inspect(status)}")
      Server.send_reply(stream, status)
      manager
    end)
  end

  def quit_worker(_, stream) do
    Logger.debug("Received quit_work")
    Logger.debug(inspect(stream.local[:main_pid]))
    send(stream.local[:main_pid], {:quit, self()})
    Grpc.Testing.Void.new()
  end
end

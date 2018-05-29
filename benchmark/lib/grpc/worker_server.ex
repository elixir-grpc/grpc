defmodule Grpc.Testing.WorkerService.Server do
  use GRPC.Server, service: Grpc.Testing.WorkerService.Service
  alias GRPC.Server

  alias Benchmark.Manager
  alias Benchmark.ServerManager
  alias Benchmark.ClientManager

  require Logger

  def run_server(args_enum, stream) do
    Enum.each(args_enum, fn args ->
      Logger.debug("Got args:")
      Logger.debug(inspect(args))

      status =
        case args.argtype do
          {:setup, config} ->
            cores = Manager.set_cores(config.core_limit)
            server = ServerManager.start_server(config)
            Logger.debug("Started server: #{inspect(server)}")
            Grpc.Testing.ServerStatus.new(stats: nil, port: server[:port], cores: cores)

          {:mark, mark} ->
            stats = get_stats(mark.reset)
            Grpc.Testing.ServerStatus.new(stats: stats)
        end

      Server.send_reply(stream, status)
    end)
  end

  def run_client(args_enum, stream) do
    Enum.each(args_enum, fn args ->
      Logger.debug("Got args:")
      Logger.debug(inspect(args))

      status =
        case args.argtype do
          {:setup, client_config} ->
            ClientManager.start_client(client_config)
            Grpc.Testing.ClientStatus.new()

          {:mark, mark} ->
            stats = get_stats(mark.reset)
            Grpc.Testing.ClientStatus.new(stats: stats)
        end

      Server.send_reply(stream, status)
    end)
  end

  def core_count(_, _) do
    Grpc.Testing.CoreResponse.new(cores: Manager.get_cores())
  end

  defp get_stats(_reset) do
  end
end

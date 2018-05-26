defmodule Grpc.Testing.WorkerService.Server do
  use GRPC.Server, service: Grpc.Testing.WorkerService.Service
  alias GRPC.Server

  def run_server(args_enum, stream) do
    Enum.each(args_enum, fn(args) ->
      status = case args.argtype do
        {:setup, server_config} ->
          server = BenchManager.start_server(server_config)
          Grpc.Testing.ServerStatus.new(stats: nil, port: server[:port], cores: server[:cores])
        {:mark, mark} ->
          stats = get_stats(mark.reset)
          Grpc.Testing.ServerStatus.new(stats: stats)
      end
      Server.send_reply(stream, status)
    end)
  end

  def core_count(req, _) do
    Grpc.Testing.CoreResponse.new(cores: BenchManager.get_cores())
  end

  defp get_stats(reset) do
  end
end

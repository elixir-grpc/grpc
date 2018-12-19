require Logger

args = System.argv()
switches = [driver_port: :integer]
{[], [], [{"--driver_port", port}]} = OptionParser.parse(args, switches: switches)
port = String.to_integer(port)

{:ok, pid, port} =
  GRPC.Server.start(Grpc.Testing.WorkerService.Server, port, local: %{main_pid: self()})

defmodule Main do
  def loop do
    receive do
      {:quit, _} ->
        Logger.debug("Got msg quit")
        Process.sleep(1000)
        GRPC.Server.stop(Grpc.Testing.WorkerService.Server)

      msg ->
        Logger.debug("Got not quit msg #{inspect(msg)}")
        Process.sleep(1000)
        loop()
    end
  end
end

Main.loop()

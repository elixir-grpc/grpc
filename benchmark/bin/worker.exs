args = System.argv()
switches = [driver_port: :integer]
{[], [], [{"--driver_port", port}]} = OptionParser.parse(args, switches: switches)
port = String.to_integer(port)

{:ok, pid, port} = GRPC.Server.start(Grpc.Testing.WorkerService.Server, port)

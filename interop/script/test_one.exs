{options, _, _} =
  OptionParser.parse(System.argv(),
    strict: [test: :string, port: :integer]
  )

test_name = Keyword.get(options, :test) || "empty_unary"
port = Keyword.get(options, :port) || 0

require Logger
Logger.configure(level: :info)

alias Interop.Client

{:ok, _pid, test_port} = GRPC.Server.start_endpoint(Interop.Endpoint, port, adapter: GRPC.Server.Adapters.ThousandIsland)
IO.puts("Server started on port #{test_port}")

opts = [adapter: GRPC.Client.Adapters.Gun]
ch = Client.connect("127.0.0.1:#{test_port}", opts)

try do
  case test_name do
    "empty_unary" -> Client.empty_unary!(ch)
    "large_unary" -> Client.large_unary!(ch)
    "client_streaming" -> Client.client_streaming!(ch)
    "server_streaming" -> Client.server_streaming!(ch)
    "ping_pong" -> Client.ping_pong!(ch)
    "empty_stream" -> Client.empty_stream!(ch)
    "custom_metadata" -> Client.custom_metadata!(ch)
    "status_code_and_message" -> Client.status_code_and_message!(ch)
    "unimplemented_service" -> Client.unimplemented_service!(ch)
    "cancel_after_begin" -> Client.cancel_after_begin!(ch)
    "timeout_on_sleeping_server" -> Client.timeout_on_sleeping_server!(ch)
    _ -> IO.puts("Unknown test: #{test_name}")
  end
  IO.puts("✓ #{test_name} PASSED")
catch
  kind, error ->
    IO.puts("✗ #{test_name} FAILED")
    IO.puts("Error: #{inspect(kind)} #{inspect(error)}")
    IO.puts(Exception.format_stacktrace(__STACKTRACE__))
end

GRPC.Server.stop_endpoint(Interop.Endpoint, adapter: GRPC.Server.Adapters.ThousandIsland)

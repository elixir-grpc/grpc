# Test custom_metadata with debug output
require Logger
Logger.configure(level: :debug)

{:ok, _pid, test_port} = GRPC.Server.start_endpoint(Interop.Endpoint, 0, adapter: GRPC.Server.Adapters.ThousandIsland)
IO.puts("Server started on port #{test_port}")

opts = [adapter: GRPC.Client.Adapters.Gun]
ch = Interop.Client.connect("127.0.0.1:#{test_port}", opts)

payload = %Grpc.Testing.Payload{body: String.duplicate(<<0>>, 271_828)}
req = %Grpc.Testing.SimpleRequest{response_size: 314_159, payload: payload}
headers = %{"x-grpc-test-echo-initial" => "test_initial_metadata_value"}
trailers = %{"x-grpc-test-echo-trailing-bin" => 0xABABAB}
metadata = Map.merge(headers, trailers)

IO.puts("\nSending request with metadata: #{inspect(metadata)}")

result = Grpc.Testing.TestService.Stub.unary_call(ch, req, metadata: metadata, return_headers: true)

IO.puts("\nReceived result:")
IO.inspect(result, label: "Result", pretty: true)

case result do
  {:ok, _reply, %{headers: recv_headers, trailers: recv_trailers}} ->
    IO.puts("\nReceived headers:")
    IO.inspect(recv_headers, label: "Headers", pretty: true)
    IO.puts("\nReceived trailers:")
    IO.inspect(recv_trailers, label: "Trailers", pretty: true)
  _ ->
    IO.puts("Unexpected result format")
end

GRPC.Server.stop_endpoint(Interop.Endpoint, adapter: GRPC.Server.Adapters.ThousandIsland)

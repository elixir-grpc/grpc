{options, _, _} = OptionParser.parse(System.argv(), strict: [rounds: :integer, concurrency: :integer, port: :integer])
rounds = Keyword.get(options, :rounds, 100)
concurrency = Keyword.get(options, :concurrency, 1)
port = Keyword.get(options, :port, 0)

IO.puts "Rounds: #{rounds}; concurrency: #{concurrency}; port: #{port}"
IO.puts ""

alias Interop.Client
{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, port)

stream = Task.async_stream(1..concurrency, fn cli ->
  ch = Client.connect("127.0.0.1", port, interceptors: [GRPCPrometheus.ClientInterceptor, GRPC.Logger.Client])
  run = fn(i) ->
    IO.puts("Client##{cli}, Round #{i}")
    Client.empty_unary!(ch)
    Client.cacheable_unary!(ch)
    Client.large_unary!(ch)
    Client.large_unary2!(ch)
    Client.client_compressed_unary!(ch)
    Client.server_compressed_unary!(ch)
    Client.client_streaming!(ch)
    Client.client_compressed_streaming!(ch)
    Client.server_streaming!(ch)
    Client.server_compressed_streaming!(ch)
    Client.ping_pong!(ch)
    Client.empty_stream!(ch)
    Client.custom_metadata!(ch)
    Client.status_code_and_message!(ch)
    Client.unimplemented_service!(ch)
    Client.cancel_after_begin!(ch)
    Client.cancel_after_first_response!(ch)
    Client.timeout_on_sleeping_server!(ch)
  end
  Enum.each(1..rounds, run)
  :ok
end, max_concurrency: concurrency, ordered: false, timeout: :infinity)

Enum.map(stream, fn result ->
  result
end)
|> IO.inspect

# defmodule Helper do
#   def flush() do
#     receive do
#      msg ->
#        IO.inspect(msg)
#        flush()
#     after
#      0 -> :ok
#     end
#   end
# end
# Helper.flush()

IO.puts("Succeed!")
:ok = GRPC.Server.stop_endpoint(Interop.Endpoint)

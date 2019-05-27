OptionParser.parse(["--rounds"], strict: [rounds: :integer])
rounds = case OptionParser.parse(System.argv(), strict: [rounds: :integer]) do
  {[rounds: rounds], [], []} ->
    rounds
  _ ->
    100
end

IO.puts "Total rounds: #{rounds}"
IO.puts ""

alias Interop.Client
port = 0
# port = 10000
{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, port)
ch = Client.connect("127.0.0.1", port, interceptors: [GRPCPrometheus.ClientInterceptor, GRPC.Logger.Client])
run = fn(i) ->
  IO.puts("Round #{i}")
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

# TODO: flush useless messages on client side
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

Enum.each(1..rounds, run)
IO.puts("Succeed!")
:ok = GRPC.Server.stop_endpoint(Interop.Endpoint)

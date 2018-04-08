alias Interop.Client
servers = [Interop.Server]
{:ok, _pid, port} = GRPC.Server.start(servers, 0)
ch = Client.connect("127.0.0.1", port)
run = fn(i) ->
  IO.puts("Round #{i}")
  Client.empty_unary!(ch)
  Client.cacheable_unary!(ch)
  Client.large_unary!(ch)
  Client.client_compressed_unary!(ch)
  Client.server_compressed_unary!(ch)
  Client.client_streaming!(ch)
  Client.client_compressed_streaming!(ch)
  Client.server_streaming!(ch)
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

Enum.each(1..100, run)
IO.puts("Succeed!")
:ok = GRPC.Server.stop(servers)

alias Interop.Client
ch = Client.connect("127.0.0.1", 50051)
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
end

Enum.each(1..100, run)
IO.puts("Succeed!")

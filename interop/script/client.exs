alias Interop.Client
ch = Client.connect("127.0.0.1", 50051)
Client.empty_unary!(ch)
IO.puts("Succeed!")

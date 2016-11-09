GRPC.Server.start(Helloworld.Greeter.Server, "localhost:50051", insecure: true)

{:ok, channel} = GRPC.Stub.connect("localhost:50051", insecure: true)
reply = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
IO.inspect reply

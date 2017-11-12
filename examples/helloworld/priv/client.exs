{:ok, channel} = GRPC.Stub.connect("localhost:50051")
{:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))
IO.inspect reply

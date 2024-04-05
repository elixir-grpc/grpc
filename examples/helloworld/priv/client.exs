{:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])

{:ok, reply} =
  channel
  |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))

# pass tuple `timeout: :infinity` as a second arg to stay in IEx debugging

IO.inspect(reply)

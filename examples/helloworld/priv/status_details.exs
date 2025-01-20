{:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Logger.Client])

{:error, reason} =
  channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "quota_failure:word"))

IO.inspect(reason)

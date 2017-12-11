defmodule Foo do
  def call(service_mod, rpc, path, channel, request, opts, next) do
    IO.puts("Foo")
    next.(service_mod, rpc, path, channel, request, opts)
  end
end

{:ok, channel} = GRPC.Stub.connect("localhost:50051")
{:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"), middlewares: [Foo])
IO.inspect reply

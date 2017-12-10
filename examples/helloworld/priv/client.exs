defmodule Foo do
  def send_request(req_stream, res_stream, stream, request, opts, next) do
    IO.puts("Foo")
    next.(req_stream, res_stream, stream, request, opts)
  end
end

{:ok, channel} = GRPC.Stub.connect("localhost:50051")
{:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"), middlewares: [Foo])
IO.inspect reply

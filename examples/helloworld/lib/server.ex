defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, stream) do
    cert = GRPC.Stream.get_cert(stream)
    IO.inspect cert
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end

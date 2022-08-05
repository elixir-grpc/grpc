defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    today = DateTime.utc_now()
    seconds = today |> DateTime.truncate(:second) |> DateTime.to_unix(:second)

    Helloworld.HelloReply.new(
      message: "Hello #{request.name}",
      today: %Google.Protobuf.Timestamp{seconds: seconds}
    )
  end
end

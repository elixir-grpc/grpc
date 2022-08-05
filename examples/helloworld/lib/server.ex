defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    today = DateTime.utc_now()
    seconds = DateTime.truncate(today, :second) |> DateTime.to_unix(:second)
    nanos = (DateTime.to_unix(today, :millisecond) |> rem(1000)) * 1_000_000

    Helloworld.HelloReply.new(
      message: "Hello #{request.name}",
      today: %Google.Protobuf.Timestamp{seconds: seconds, nanos: nanos}
    )
  end
end

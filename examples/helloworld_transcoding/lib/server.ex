defmodule Helloworld.Greeter.Server do
  use GRPC.Server,
    service: Helloworld.Greeter.Service,
    http_transcode: true

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(
      message: "Hello #{request.name}",
      today: today()
    )
  end

  @spec say_hello_from(Helloworld.HelloFromRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello_from(request, _stream) do
    Helloworld.HelloReply.new(
      message: "Hello #{request.name}. From #{request.from}",
      today: today()
    )
  end

  defp today do
    nanos_epoch = System.system_time() |> System.convert_time_unit(:native, :nanosecond)
    seconds = div(nanos_epoch, 1_000_000_000)
    nanos = nanos_epoch - seconds * 1_000_000_000

    %Google.Protobuf.Timestamp{seconds: seconds, nanos: nanos}
  end
end

defmodule LoggerEvent do
  require Logger

  def handle_event(event, measurements, metadata, config) do
    Logger.info("""
    Receive event,

      Event: #{inspect(event)},
      Measurements: #{inspect(measurements)},
      Metadata: #{inspect(metadata)}
    """)
  end
end

prefix = [:grpc, :client, :request]

:telemetry.attach_many(
  "ClientEvent",
  [
    prefix ++ [:start],
    prefix ++ [:stop],
    prefix ++ [:exception],
    prefix ++ [:recv_headers],
  ],
  &LoggerEvent.handle_event/4,
  %{}
)

{:ok, channel} = GRPC.Stub.connect("localhost:50051")

{:ok, reply} =
  channel |> Helloworld.Greeter.Stub.say_hello(Helloworld.HelloRequest.new(name: "grpc-elixir"))

IO.inspect(reply)

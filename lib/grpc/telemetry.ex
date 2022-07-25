defmodule GRPC.Telemetry do

  def metadata_from_stream(stream) do
    %{
      stream: stream,
      service_name: stream.service_name,
      method_name: stream.method_name,
      host: stream.channel.host,
      port: stream.channel.port
    }
  end
end

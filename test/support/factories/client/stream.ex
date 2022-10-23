defmodule GRPC.Factories.Client.Stream do
  defmacro __using__(_opts) do
    quote do
      def client_stream_factory do
        %GRPC.Client.Stream{
          __interface__: %{
            receive_data: &GRPC.Client.Stream.receive_data/2,
            send_request: &GRPC.Client.Stream.send_request/3
          },
          canceled: false,
          channel: build(:channel, adapter: GRPC.Client.Adapters.Mint),
          codec: GRPC.Codec.Proto,
          compressor: nil,
          grpc_type: :unary,
          headers: %{},
          method_name: "SayHello",
          path: "/helloworld.Greeter/SayHello",
          payload: %{},
          request_mod: Helloworld.HelloRequest,
          response_mod: Helloworld.HelloReply,
          rpc: {"say_hello", {Helloworld.HelloRequest, false}, {Helloworld.HelloReply, false}},
          server_stream: false,
          service_name: "helloworld.Greeter",
          accepted_compressors: [GRPC.Compressor.Gzip]
        }
      end
    end
  end
end

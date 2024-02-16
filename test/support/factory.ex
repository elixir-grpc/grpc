defmodule GRPC.Factory do
  @moduledoc false

  alias GRPC.Channel
  alias GRPC.Credential

  def build(resource, attrs \\ %{}) do
    name = :"#{resource}_factory"

    attrs = Map.new(attrs)

    data =
      if function_exported?(__MODULE__, name, 1) do
        apply(__MODULE__, name, [attrs])
      else
        apply(__MODULE__, name, [])
      end

    Map.merge(data, attrs)
  end

  def channel_factory do
    %Channel{
      host: "localhost",
      port: 1337,
      scheme: "http",
      cred: build(:credential),
      adapter: GRPC.Client.Adapters.Gun,
      adapter_payload: %{},
      codec: GRPC.Codec.Proto,
      interceptors: [],
      compressor: nil,
      accepted_compressors: [],
      headers: []
    }
  end

  def credential_factory do
    cert_path = Path.expand("./tls/server1.pem", :code.priv_dir(:grpc))
    key_path = Path.expand("./tls/server1.key", :code.priv_dir(:grpc))
    ca_path = Path.expand("./tls/ca.pem", :code.priv_dir(:grpc))

    %Credential{
      ssl: [
        certfile: cert_path,
        cacertfile: ca_path,
        keyfile: key_path,
        verify: :verify_none,
        versions: [:"tlsv1.2", :"tlsv1.3"]
      ]
    }
  end

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

  def hello_reply_rpc_factory do
    %Helloworld.HelloReply{message: "Hello Luis"}
  end
end

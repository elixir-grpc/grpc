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
        versions: [:"tlsv1.2"]
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

  def grpc_status_codes_messages_factory do
    [
      {:ok, 0, nil},
      {:cancelled, 1, "The operation was cancelled (typically by the caller)"},
      {:unknown, 2, "Unknown error"},
      {:invalid_argument, 3, "Client specified an invalid argument"},
      {:deadline_exceeded, 4, "Deadline expired before operation could complete"},
      {:not_found, 5, "Some requested entity (e.g., file or directory) was not found"},
      {:already_exists, 6,
       "Some entity that we attempted to create (e.g., file or directory) already exists"},
      {:permission_denied, 7,
       "The caller does not have permission to execute the specified operation"},
      {:resource_exhausted, 8, "Some resource has been exhausted"},
      {:failed_precondition, 9,
       "Operation was rejected because the system is not in a state required for the operation's execution"},
      {:aborted, 10, "The operation was aborted"},
      {:out_of_range, 11, "Operation was attempted past the valid range"},
      {:unimplemented, 12,
       "Operation is not implemented or not supported/enabled in this service"},
      {:internal, 13, "Internal errors"},
      {:unavailable, 14, "The service is currently unavailable"},
      {:data_loss, 15, "Unrecoverable data loss or corruption"},
      {:unauthenticated, 16,
       "The request does not have valid authentication credentials for the operation"}
    ]
  end
end

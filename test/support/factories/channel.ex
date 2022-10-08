defmodule GRPC.Factories.Channel do
  alias GRPC.Channel
  alias GRPC.Credential

  defmacro __using__(_opts) do
    quote do
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
            verify: :verify_peer,
            fail_if_no_peer_cert: true,
            versions: [:"tlsv1.2"]
          ]
        }
      end
    end
  end
end

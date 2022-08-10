defmodule GRPC.Factory do
  @moduledoc false

  alias GRPC.Channel
  alias GRPC.Credential

  @cert_path Path.expand("./tls/server1.pem", :code.priv_dir(:grpc))
  @key_path Path.expand("./tls/server1.key", :code.priv_dir(:grpc))
  @ca_path Path.expand("./tls/ca.pem", :code.priv_dir(:grpc))

  def build(resource, attrs \\ %{}) do
    name = :"#{resource}_factory"

    data =
      if function_exported?(__MODULE__, name, 1) do
        apply(__MODULE__, name, [attrs])
      else
        apply(__MODULE__, name, [])
      end

    Map.merge(data, Map.new(attrs))
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
    %Credential{
      ssl: [
        certfile: @cert_path,
        cacertfile: @ca_path,
        keyfile: @key_path,
        verify: :verify_peer,
        fail_if_no_peer_cert: true,
        versions: [:"tlsv1.2"]
      ]
    }
  end
end

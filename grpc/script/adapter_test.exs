alias GRPC.Client.Adapters.Gun
alias GRPC.Client.Adapters.Mint
alias GRPC.Channel
alias GRPC.Credential

require Logger

{adapter_to_test, dependency} =
  case System.argv() do
    ["gun" | _rest] -> {Gun, :gun}
    ["mint" | _rest] -> {Mint, :mint}
  end

Mix.install([
  dependency,
  {:grpc, path: Path.join(__DIR__, "..")},
  {:grpc_core, path: Path.join(__DIR__, "../../grpc_core"), override: true},
  {:grpc_server, path: Path.join(__DIR__, "../../grpc_server")}
])

defmodule FeatureServer.Service do
  use GRPC.Service, name: "FeatureServer", protoc_gen_elixir_version: "0.16.0"
end

defmodule FeatureServer do
  use GRPC.Server, service: FeatureServer.Service
end

supported_adapters = [Gun, Mint]

Logger.info("Testing #{adapter_to_test}...")

build_credential = fn ->
  cert_path = Path.expand("./tls/server1.pem", :code.priv_dir(:grpc))
  key_path = Path.expand("./tls/server1.key", :code.priv_dir(:grpc))
  ca_path = Path.expand("./tls/ca.pem", :code.priv_dir(:grpc))

  struct(
    Credential,
    ssl: [
      certfile: cert_path,
      cacertfile: ca_path,
      keyfile: key_path,
      verify: :verify_none,
      versions: [:"tlsv1.2", :"tlsv1.3"]
    ]
  )
end

server_credential = build_credential.()

server_opts = %{
  gun: [adapter_opts: [cred: server_credential]],
  mint: []
}

build_channel = fn adapter, port ->
  struct(
    Channel,
    host: "localhost",
    port: port,
    scheme: "http",
    cred: server_credential,
    adapter: adapter,
    adapter_payload: %{},
    codec: GRPC.Codec.Proto,
    interceptors: [],
    compressor: nil,
    accepted_compressors: [],
    headers: []
  )
end

{:ok, _, port} = GRPC.Server.start(FeatureServer, 0, Map.get(server_opts, dependency, []))

for adapter <- supported_adapters do
  channel = build_channel.(adapter, port)

  if adapter == adapter_to_test do
    {:ok, _} = adapter.connect(channel, [])
  else
    try do
      {:ok, _} = adapter.connect(channel, [])
      raise "#{adapter} should not be available!"
    rescue
      UndefinedFunctionError ->
        :ok

      e ->
        :ok = GRPC.Server.stop(FeatureServer)
        raise e
    after
    end
  end
end

Logger.info("Success!")
:ok = GRPC.Server.stop(FeatureServer)

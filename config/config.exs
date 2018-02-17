use Mix.Config

config :grpc,
  http2_client_adapter: GRPC.Adapter.Chatterbox.Client

if Mix.env == :test do
  import_config "#{Mix.env}.exs"
end

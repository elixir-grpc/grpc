use Mix.Config

config :grpc,
  http2_client_adapter: GRPC.Adapter.Gun

if Mix.env == :test do
  import_config "#{Mix.env}.exs"
end

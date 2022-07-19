import Config

config :grpc, http2_client_adapter: GRPC.Adapter.Gun

config_file = Path.expand("#{config_env()}.exs", __DIR__)

if File.exists?(config_file) do
  import_config config_file
end

use Mix.Config

config :lager,
  handlers: [
    lager_console_backend: :warn
  ]

config :grpc,
  http2_client_adapter: GRPC.Adapter.Chatterbox.Client

if Mix.env == :test do
  import_config "#{Mix.env}.exs"
end

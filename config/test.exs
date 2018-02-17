use Mix.Config

http2_client_adapter =
  case System.get_env("CLIENT_ADAPTER") do
    "gun" -> GRPC.Adapter.Gun
    "chatterbox" -> GRPC.Adapter.Chatterbox.Client
    _ -> GRPC.Adapter.Chatterbox.Client
  end

config :grpc,
  http2_client_adapter: http2_client_adapter

use Mix.Config

config :grpc, http2_client_adapter: GRPC.Client.Adapters.Gun

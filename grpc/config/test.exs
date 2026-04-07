import Config

config :logger, level: :info

config :grpc_client, :dns_adapter, GRPC.Client.Resolver.DNS.MockAdapter

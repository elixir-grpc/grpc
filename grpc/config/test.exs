import Config

config :logger, level: :info

config :grpc, :dns_adapter, GRPC.Client.Resolver.DNS.MockAdapter

import Config

config :prometheus, GRPCPrometheus.ServerInterceptor,
  latency: :histogram

config :prometheus, GRPCPrometheus.ClientInterceptor,
  latency: :histogram

# config :grpc, start_server: true

config :logger, level: :warn

import Config

config :prometheus, GRPCPrometheus.ServerInterceptor,
  latency: :histogram

config :prometheus, GRPCPrometheus.ClientInterceptor,
  latency: :histogram

config :logger, level: :warn

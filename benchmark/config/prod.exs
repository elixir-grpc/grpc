use Mix.Config

config :logger, :console, metadata: [:module, :line]

config :logger,
  level: :warn

config :sasl, errlog_type: :error

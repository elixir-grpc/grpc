use Mix.Config

config :logger, :console, metadata: [:module, :line]

config :sasl, errlog_type: :error

use Mix.Config

config :logger, :console, metadata: [:module, :line]
config :logger, level: :info

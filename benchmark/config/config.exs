import Config

# Disable logging by default for better performance
config :logger, level: :error

import_config "#{Mix.env()}.exs"

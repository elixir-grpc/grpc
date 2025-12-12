import Config

# Reduce logging overhead by default for better performance
config :logger, level: :error

import_config "#{Mix.env()}.exs"

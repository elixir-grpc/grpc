import Config

config :logger, level: :error

import_config "#{Mix.env()}.exs"

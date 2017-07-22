use Mix.Config

if Mix.env == :dev do
  config :mix_test_watch,
    tasks: [
        "coveralls",
        "credo -a --format=oneline",
      ]
end

config :lager, handlers: [
  lager_console_backend: :warn
]

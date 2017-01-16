use Mix.Config

config :lager, handlers: [
  lager_console_backend: :warn
]

# Start server in OTP
# config :helloworld,
#   start_server: true

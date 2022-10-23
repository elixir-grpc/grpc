{options, _, _} = OptionParser.parse(System.argv(), strict: [rounds: :integer, concurrency: :integer, port: :integer])
rounds = Keyword.get(options, :rounds) || 20
max_concurrency = System.schedulers_online()
concurrency = Keyword.get(options, :concurrency) || max_concurrency
port = Keyword.get(options, :port) || 0
level = Keyword.get(options, :log_level) || "warn"
level = String.to_existing_atom(level)

require Logger

Logger.configure(level: level)

Logger.info("Rounds: #{rounds}; concurrency: #{concurrency}; port: #{port}")

alias Interop.Client
alias GRPC.Client.Adapters.{Mint, Gun}

{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, port)

for adapter <- [Gun, Mint] do
  1..concurrency
  |> Task.async_stream(fn _cli ->
                          ch = Client.connect("127.0.0.1", port, interceptors: [GRPCPrometheus.ClientInterceptor, GRPC.Logger.Client], adapter: adapter)

                          for _ <- 1..rounds do
                            Client.empty_unary!(ch)
                            Client.cacheable_unary!(ch)
                            Client.large_unary!(ch)
                            Client.large_unary2!(ch)
                            Client.client_compressed_unary!(ch)
                            Client.server_compressed_unary!(ch)
                            Client.client_streaming!(ch)
                            Client.client_compressed_streaming!(ch)
                            Client.server_streaming!(ch)
                            Client.server_compressed_streaming!(ch)
                            Client.ping_pong!(ch)
                            Client.empty_stream!(ch)
                            Client.custom_metadata!(ch)
                            Client.status_code_and_message!(ch)
                            Client.unimplemented_service!(ch)
                            Client.cancel_after_begin!(ch)
                            Client.cancel_after_first_response!(ch)
                            Client.timeout_on_sleeping_server!(ch)
                          end
                          :ok
  end, max_concurrency: concurrency, ordered: false, timeout: :infinity)
  |> Enum.to_list()
end

Logger.info("Succeed!")
:ok = GRPC.Server.stop_endpoint(Interop.Endpoint)

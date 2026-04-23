{options, _, _} =
  OptionParser.parse(System.argv(),
    strict: [rounds: :integer, concurrency: :integer, port: :integer, level: :string, adapter: :string]
  )

rounds = Keyword.get(options, :rounds) || 20
max_concurrency = System.schedulers_online()
concurrency = Keyword.get(options, :concurrency) || max_concurrency
port = Keyword.get(options, :port) || 0
level = Keyword.get(options, :level) || "warning"
level = String.to_existing_atom(level)

alias GRPC.Client.Adapters.{Gun, Mint}

{adapter, missing_adapters} = case Keyword.get(options, :adapter, "gun") do
  "gun" ->
    Mix.install([:gun])
    {Gun, [Mint]}
  "mint" ->
    Mix.install([:mint])
    {Mint, [Gun]}
end

require Logger

Logger.configure(level: level)

Logger.info("Rounds: #{rounds}; concurrency: #{concurrency}; port: #{port}; adapter: #{adapter}")

alias Interop.Client

{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, port)

defmodule InteropTestRunner do
  def run(_cli, adapter, port, rounds) do
    opts = [interceptors: [GRPC.Client.Interceptors.Logger], adapter: adapter]
    ch = Client.connect("127.0.0.1:#{port}", opts)

    for round <- 1..rounds do
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

      IO.inspect(round, label: "Round #{round} --------------------------------")
    end

    :ok
  end
end

res = DynamicSupervisor.start_link(strategy: :one_for_one, name: GRPC.Client.Supervisor)

{:ok, _pid} =
  case res do
    {:ok, pid} ->
      {:ok, pid}

    {:error, {:already_started, pid}} ->
      {:ok, pid}
  end

Logger.info("Starting run for adapter: #{adapter}")
args = [adapter, port, rounds]
stream_opts = [max_concurrency: concurrency, ordered: false, timeout: :infinity]

1..concurrency
|> Task.async_stream(InteropTestRunner, :run, args, stream_opts)
|> Enum.to_list()

for missing <- missing_adapters do
  Logger.info("Checking to make sure #{missing} cannot be used...")
  try do
    InteropTestRunner.run(missing, port, rounds)
    raise "#{missing} should not be present"
  catch
    UndefinedFunctionError -> :ok
    e ->
      :ok = GRPC.Server.stop_endpoint(Interop.Endpoint)
      raise e
  end
end

Logger.info("Succeed!")
:ok = GRPC.Server.stop_endpoint(Interop.Endpoint)

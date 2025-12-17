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
server_adapter_name = Keyword.get(options, :adapter) || "both"

require Logger

Logger.configure(level: level)

Logger.info("Rounds: #{rounds}; concurrency: #{concurrency}; port: #{port}; server_adapter: #{server_adapter_name}")

alias GRPC.Client.Adapters.Gun
alias GRPC.Client.Adapters.Mint
alias Interop.Client

# Determine which server adapters to test
server_adapters = case server_adapter_name do
  "cowboy" -> [GRPC.Server.Adapters.Cowboy]
  "thousand_island" -> [GRPC.Server.Adapters.ThousandIsland]
  "both" -> [GRPC.Server.Adapters.Cowboy, GRPC.Server.Adapters.ThousandIsland]
  _ ->
    IO.puts("Unknown adapter: #{server_adapter_name}. Using both.")
    [GRPC.Server.Adapters.Cowboy, GRPC.Server.Adapters.ThousandIsland]
end

client_adapters = [Gun, Mint]

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

# Test each server adapter
for server_adapter <- server_adapters do
  server_name = server_adapter |> Module.split() |> List.last()
  Logger.info("========================================")
  Logger.info("Testing with SERVER ADAPTER: #{server_name}")
  Logger.info("========================================")

  {:ok, _pid, test_port} = GRPC.Server.start_endpoint(Interop.Endpoint, port, adapter: server_adapter)
  Logger.info("Server started on port #{test_port}")
  # Give server time to fully initialize
  Process.sleep(100)

  for client_adapter <- client_adapters do
    client_name = client_adapter |> Module.split() |> List.last()
    Logger.info("Starting run for client adapter: #{client_name}")
    args = [client_adapter, test_port, rounds]
    stream_opts = [max_concurrency: concurrency, ordered: false, timeout: :infinity]

    1..concurrency
    |> Task.async_stream(InteropTestRunner, :run, args, stream_opts)
    |> Enum.to_list()
  end

  :ok = GRPC.Server.stop_endpoint(Interop.Endpoint, adapter: server_adapter)
  Logger.info("Completed tests for #{server_name}")
end

Logger.info("All tests succeeded!")
:ok

{options, _, _} = OptionParser.parse(System.argv(), strict: [rounds: :integer, concurrency: :integer, port: :integer, level: :string])
rounds = Keyword.get(options, :rounds) || 20
max_concurrency = System.schedulers_online()
concurrency = Keyword.get(options, :concurrency) || max_concurrency
port = Keyword.get(options, :port) || 0
level = Keyword.get(options, :level) || "warning"
level = String.to_existing_atom(level)

require Logger

Logger.configure(level: level)

Logger.info("Rounds: #{rounds}; concurrency: #{concurrency}; port: #{port}")

alias GRPC.Client.Adapters.Gun
alias GRPC.Client.Adapters.Mint
alias GRPC.Client.Adapters.Finch
alias Interop.Client

{:ok, _pid, port} = GRPC.Server.start_endpoint(Interop.Endpoint, port)

defmodule InteropTestRunner do
  def run(_cli, adapter, port, rounds) do
    opts = [interceptors: [GRPC.Client.Interceptors.Logger], adapter: adapter]
    ch = Client.connect("127.0.0.1", port, opts) |> IO.inspect()

    for _ <- 1..rounds do
      # Client.empty_unary!(ch)
      # Client.cacheable_unary!(ch)
      # Client.large_unary!(ch)
      # Client.large_unary2!(ch)
      # Client.client_compressed_unary!(ch)
      # Client.server_compressed_unary!(ch)
      Client.client_streaming!(ch)
      # Client.client_compressed_streaming!(ch)
      # Client.server_streaming!(ch)
      # Client.server_compressed_streaming!(ch)
        # Client.ping_pong!(ch)
      # Client.empty_stream!(ch)
        # Client.custom_metadata!(ch)
      # Client.status_code_and_message!(ch)
      # Client.unimplemented_service!(ch)
      # Client.cancel_after_begin!(ch)
        # Client.cancel_after_first_response!(ch)
        # Client.timeout_on_sleeping_server!(ch)
    end
    :ok
  end
end

# %GRPC.Channel{
#   host: "127.0.0.1",
#   port: 36541,
#   scheme: "http",
#   cred: nil,
#   adapter: GRPC.Client.Adapters.Finch,
#   adapter_payload: %{conn_pid: #PID<0.272.0>},
#   codec: GRPC.Codec.Proto,
#   interceptors: [{GRPC.Client.Interceptors.Logger, [level: :info]}],
#   compressor: nil,
#   accepted_compressors: [],
#   headers: []
# }

# %GRPC.Channel{
#   host: "127.0.0.1",
#   port: 46567,
#   scheme: "http",
#   cred: nil,
#   adapter: GRPC.Client.Adapters.Mint,
#   adapter_payload: %{conn_pid: #PID<0.272.0>},
#   codec: GRPC.Codec.Proto,
#   interceptors: [{GRPC.Client.Interceptors.Logger, [level: :info]}],
#   compressor: nil,
#   accepted_compressors: [],
#   headers: []
# }


# request: %GRPC.Client.Stream{
#   channel: %GRPC.Channel{
#     host: "127.0.0.1",
#     port: 36913,
#     scheme: "http",
#     cred: nil,
#     adapter: GRPC.Client.Adapters.Mint,
#     adapter_payload: %{conn_pid: #PID<0.272.0>},
#     codec: GRPC.Codec.Proto,
#     interceptors: [{GRPC.Client.Interceptors.Logger, [level: :info]}],
#     compressor: nil,
#     accepted_compressors: [],
#     headers: []
#   },
#   service_name: "grpc.testing.TestService",
#   method_name: "EmptyCall",
#   grpc_type: :unary,
#   rpc: {"empty_call", {Grpc.Testing.Empty, false}, {Grpc.Testing.Empty, false},
#    %{}},
#   payload: %{},
#   path: "/grpc.testing.TestService/EmptyCall",
#   request_mod: Grpc.Testing.Empty,
#   response_mod: Grpc.Testing.Empty,
#   codec: GRPC.Codec.Proto,
#   server_stream: false,
#   canceled: false,
#   compressor: nil,
#   accepted_compressors: [],
#   headers: %{},
#   __interface__: %{
#     send_request: &GRPC.Client.Stream.send_request/3,
#     receive_data: &GRPC.Client.Stream.receive_data/2
#   }
# }

# request: %GRPC.Client.Stream{
#   channel: %GRPC.Channel{
#     host: "127.0.0.1",
#     port: 42075,
#     scheme: "http",
#     cred: nil,
#     adapter: GRPC.Client.Adapters.Finch,
#     adapter_payload: %{conn_pid: #PID<0.353.0>},
#     codec: GRPC.Codec.Proto,
#     interceptors: [{GRPC.Client.Interceptors.Logger, [level: :info]}],
#     compressor: nil,
#     accepted_compressors: [],
#     headers: []
#   },
#   service_name: "grpc.testing.TestService",
#   method_name: "EmptyCall",
#   grpc_type: :unary,
#   rpc: {"empty_call", {Grpc.Testing.Empty, false}, {Grpc.Testing.Empty, false},
#    %{}},
#   payload: %{},
#   path: "/grpc.testing.TestService/EmptyCall",
#   request_mod: Grpc.Testing.Empty,
#   response_mod: Grpc.Testing.Empty,
#   codec: GRPC.Codec.Proto,
#   server_stream: false,
#   canceled: false,
#   compressor: nil,
#   accepted_compressors: [],
#   headers: %{},
#   __interface__: %{
#     send_request: &GRPC.Client.Stream.send_request/3,
#     receive_data: &GRPC.Client.Stream.receive_data/2
#   }
# }

for adapter <- [Finch] do
  Logger.info("Starting run for adapter: #{adapter}")
  args = [adapter, port, rounds]
  stream_opts = [max_concurrency: concurrency, ordered: false, timeout: :infinity]
  1..concurrency
  |> Task.async_stream(InteropTestRunner, :run, args, stream_opts)
  |> Enum.to_list()
end

Logger.info("Succeed!")
:ok = GRPC.Server.stop_endpoint(Interop.Endpoint)

alias Benchmark.ServerManager
alias Benchmark.ClientManager

server =
  Grpc.Testing.ServerConfig.new(
    async_server_threads: 1,
    channel_args: [
      Grpc.Testing.ChannelArg.new(
        name: "grpc.optimization_target",
        value: {:str_value, "latency"}
      )
    ]
  )

server = ServerManager.start_server(server)

client =
  Grpc.Testing.ClientConfig.new(
    async_client_threads: 1,
    channel_args: [
      Grpc.Testing.ChannelArg.new(
        name: "grpc.optimization_target",
        value: {:str_value, "latency"}
      )
    ],
    client_channels: 1,
    histogram_params: Grpc.Testing.HistogramParams.new(max_possible: 6.0e10, resolution: 0.01),
    load_params:
      Grpc.Testing.LoadParams.new(load: {:closed_loop, Grpc.Testing.ClosedLoopParams.new()}),
    outstanding_rpcs_per_channel: 1,
    payload_config:
      Grpc.Testing.PayloadConfig.new(
        payload: {:simple_params, Grpc.Testing.SimpleProtoParams.new()}
      ),
    server_targets: ["localhost:#{server.port}"]
  )

manager = ClientManager.start_client(client)

Process.sleep(5000)

stats = ClientManager.get_stats(manager, false)

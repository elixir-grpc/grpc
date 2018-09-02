server =
  Grpc.Testing.ServerConfig.new(
    async_server_threads: 1,
    port: 10000,
    channel_args: [
      Grpc.Testing.ChannelArg.new(
        name: "grpc.optimization_target",
        value: {:str_value, "latency"}
      )
    ]
  )

server = Benchmark.ServerManager.start_server(server)

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
    server_targets: ["localhost:10000"]
  )

manager = Benchmark.ClientManager.start_client(client)

payload_type = Grpc.Testing.PayloadType.value(:COMPRESSABLE)
resp_size = 0
req_size = 0

payload =
  Grpc.Testing.Payload.new(
    type: payload_type,
    body: List.duplicate(<<0>>, req_size)
  )

req =
  Grpc.Testing.SimpleRequest.new(
    response_type: payload_type,
    response_size: resp_size,
    payload: payload
  )

{:ok, ch} = GRPC.Stub.connect("localhost:10000")
Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req)

# IO.inspect(Time.utc_now()); Enum.each(1..1000, fn _ -> Grpc.Testing.BenchmarkService.Stub.unary_call(ch, req) end); IO.inspect(Time.utc_now())

Process.sleep(500)

stats = Benchmark.ClientManager.get_stats(manager, false)
IO.inspect(stats)

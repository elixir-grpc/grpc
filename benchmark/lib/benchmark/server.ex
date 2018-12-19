defmodule Benchmark.Server do
  defstruct cores: nil,
            port: nil,
            pid: nil,
            init_time: nil,
            init_rusage: nil

  def get_stats(server, reset \\ false) do
    {server, stats} = Benchmark.Stats.CpuTime.get_stats(server, reset)
    stats = Grpc.Testing.ServerStats.new(stats)
    {server, stats}
  end
end

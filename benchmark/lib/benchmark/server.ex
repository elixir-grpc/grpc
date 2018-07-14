defmodule Benchmark.Server do
  defstruct cores: nil,
            port: nil,
            pid: nil,
            init_time: nil,
            cpu_acc: %{sys_t: 0, user_t: 0}

  def get_stats(server, reset \\ false) do
    {server, stats} = Benchmark.Stats.CpuTime.get_stats(server, reset)
    stats = Grpc.Testing.ServerStats.new(stats)
    {server, stats}
  end
end

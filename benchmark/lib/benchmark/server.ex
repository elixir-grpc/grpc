defmodule Benchmark.Server do
  defstruct cores: nil,
            port: nil,
            pid: nil,
            init_time: nil,
            cpu_acc: %{sys_t: 0, user_t: 0},
            init_util: nil

  def get_stats(server, reset \\ false) do
    now = Time.utc_now()
    util = :cpu_sup.util([:detailed])
    duration = Time.diff(now, server.init_time, :microsecond) / 1000_000
    {_, [kernel: sys_t, nice_user: _, user: user_t], _, _} = util
    %{sys_t: sys_t0, user_t: user_t0} = server.cpu_acc
    sys_t = sys_t0 + sys_t
    user_t = user_t0 + user_t

    server =
      if reset do
        %{server | init_time: Time.utc_now(), cpu_acc: %{sys_t: 0, user_t: 0}}
      else
        %{server | cpu_acc: %{sys_t: sys_t, user_t: user_t}}
      end

    stats =
      Grpc.Testing.ServerStats.new(
        time_elapsed: duration,
        time_user: user_t,
        time_system: sys_t
      )

    {server, stats}
  end
end

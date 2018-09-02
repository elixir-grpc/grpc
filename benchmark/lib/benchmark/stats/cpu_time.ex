defmodule Benchmark.Stats.CpuTime do
  def get_stats(state, reset \\ false) do
    now = Time.utc_now()
    rusage = Benchmark.Syscall.getrusage()
    duration = Time.diff(now, state.init_time, :microsecond) / 1000_000

    {utime, stime} = cpu_time_diff(rusage, state.init_rusage)

    state =
      if reset do
        %{state | init_time: Time.utc_now(), init_rusage: rusage}
      else
        state
      end

    stats = %{
      time_elapsed: duration,
      time_user: utime,
      time_system: stime
    }

    {state, stats}
  end

  def cpu_time_diff(
        %{utime: {u_sec1, u_usec1}, stime: {s_sec1, s_usec1}},
        %{utime: {u_sec0, u_usec0}, stime: {s_sec0, s_usec0}}
      ) do
    utime = u_sec1 - u_sec0 + (u_usec1 - u_usec0) / 1000_000
    stime = s_sec1 - s_sec0 + (s_usec1 - s_usec0) / 1000_000
    {utime, stime}
  end
end

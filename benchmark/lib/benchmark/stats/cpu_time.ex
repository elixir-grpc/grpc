defmodule Benchmark.Stats.CpuTime do
  def get_stats(state, reset \\ false) do
    now = Time.utc_now()
    util = :cpu_sup.util([:detailed])
    duration = Time.diff(now, state.init_time, :microsecond) / 1000_000
    {_, [kernel: sys_t, nice_user: _, user: user_t], _, _} = util
    %{sys_t: sys_t0, user_t: user_t0} = state.cpu_acc
    sys_t = sys_t0 + sys_t
    user_t = user_t0 + user_t

    state =
      if reset do
        %{state | init_time: Time.utc_now(), cpu_acc: %{sys_t: 0, user_t: 0}}
      else
        %{state | cpu_acc: %{sys_t: sys_t, user_t: user_t}}
      end

    stats = %{
      time_elapsed: duration,
      time_user: user_t,
      time_system: sys_t
    }

    {state, stats}
  end
end

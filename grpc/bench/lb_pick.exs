# Benchmark: per-request pick throughput for the client load balancer.
#
# Compares three strategies:
#   1. persistent_term.get — the pre-refactor pick_channel read path
#   2. ETS RoundRobin.pick — the current lock-free round-robin pick
#   3. GenServer.call      — upper-bound cost of serialising picks through
#                            a process mailbox (the anti-pattern)
#
# Also measures concurrent pick throughput with many client processes
# hitting the same LB, which is representative of RPC traffic.
#
# Run with:  mix run bench/lb_pick.exs

alias GRPC.Channel
alias GRPC.Client.LoadBalancing.RoundRobin

# Build N synthetic channels.
build_channels = fn n ->
  for i <- 1..n do
    %Channel{host: "backend-#{i}", port: 50050 + i, ref: {:ref, i}}
  end
end

channels = build_channels.(4)

# --- Setup for each strategy ---------------------------------------------

# 1. persistent_term — stores a single pre-picked channel, like the old code
pt_key = {__MODULE__, :pick, :bench}
:persistent_term.put(pt_key, hd(channels))

# 2. ETS RoundRobin
{:ok, rr_state} = RoundRobin.init(channels: channels)

# 3. GenServer serving picks from its mailbox
defmodule PickServer do
  use GenServer

  def start_link(channels), do: GenServer.start_link(__MODULE__, channels)
  def pick(pid), do: GenServer.call(pid, :pick)

  @impl true
  def init(channels), do: {:ok, {List.to_tuple(channels), 0}}

  @impl true
  def handle_call(:pick, _from, {tuple, idx}) do
    ch = elem(tuple, rem(idx, tuple_size(tuple)))
    {:reply, {:ok, ch}, {tuple, idx + 1}}
  end
end

{:ok, gs_pid} = PickServer.start_link(channels)

# --- Benchmarks ----------------------------------------------------------

IO.puts("\n=== Single-process pick throughput ===\n")

Benchee.run(
  %{
    "persistent_term.get" => fn ->
      :persistent_term.get(pt_key)
    end,
    "ETS RoundRobin.pick" => fn ->
      RoundRobin.pick(rr_state)
    end,
    "GenServer.call pick" => fn ->
      PickServer.pick(gs_pid)
    end
  },
  time: 3,
  warmup: 1,
  print: [fast_warning: false]
)

IO.puts("\n=== 32-process concurrent pick throughput ===\n")

Benchee.run(
  %{
    "persistent_term.get" => fn ->
      :persistent_term.get(pt_key)
    end,
    "ETS RoundRobin.pick" => fn ->
      RoundRobin.pick(rr_state)
    end,
    "GenServer.call pick" => fn ->
      PickServer.pick(gs_pid)
    end
  },
  time: 3,
  warmup: 1,
  parallel: 32,
  print: [fast_warning: false]
)

# --- Write-path cost: reconciliation --------------------------------------
#
# persistent_term.put triggers a global GC pass across every BEAM process.
# ETS :ets.insert is local to the table. This is the decisive difference
# under frequent DNS re-resolution.

IO.puts("\n=== Reconciliation write cost ===\n")

new_channels = build_channels.(8)

Benchee.run(
  %{
    "persistent_term.put" => fn ->
      :persistent_term.put(pt_key, hd(new_channels))
    end,
    "ETS RoundRobin.update" => fn ->
      RoundRobin.update(rr_state, new_channels)
    end
  },
  time: 3,
  warmup: 1,
  print: [fast_warning: false]
)

# Cleanup
RoundRobin.shutdown(rr_state)
:persistent_term.erase(pt_key)
GenServer.stop(gs_pid)

defmodule GRPC.Client.Adapters.Mint.KeepAliveTimer do
  # This file is a copy of https://github.com/NFIBrokerage/spear/blob/9177196721d943fda2ee1a70698580b036f220ee/lib/spear/connection/keep_alive_timer.ex
  @moduledoc """
  Curates the keep-alive timer for a Mint-controlled channel connection.

  gRPC keep-alive is just HTTP2 keep-alive - send PING frames every interval
  after receiving some data and wait at most timeout before saying the
  connection is severed

  note that `:interval_timer` is not an actual interval timer as with
  `:timer.send_interval/3`. See https://github.com/NFIBrokerage/spear/issues/10
  for info on keep-alive conformity
  """

  defstruct interval_timer: nil, interval: 10_000, timeout: 10_000, timeout_timers: %{}

  def start(%{keep_alive_interval: interval, keep_alive_timeout: timeout})
      when interval == false or timeout == false,
      do: %__MODULE__{}

  def start(%{keep_alive_interval: interval, keep_alive_timeout: timeout}) do
    %__MODULE__{
      interval_timer: start_interval_timer(interval),
      interval: interval,
      timeout: timeout
    }
  end

  def reset_interval_timer(%__MODULE__{} = keep_alive_timer) do
    cancel_timer(keep_alive_timer.interval_timer)

    %__MODULE__{
      keep_alive_timer
      | interval_timer: start_interval_timer(keep_alive_timer.interval)
    }
  end

  def clear(%__MODULE__{interval_timer: interval_timer, timeout_timers: timeout_timers}) do
    cancel_timer(interval_timer)

    :ok = Enum.each(timeout_timers, fn {_request_ref, timer} -> cancel_timer(timer) end)

    %__MODULE__{}
  end

  def start_timeout_timer(%__MODULE__{} = keep_alive_timer, request_ref) do
    put_in(
      keep_alive_timer.timeout_timers[request_ref],
      Process.send_after(self(), :keep_alive_expired, keep_alive_timer.timeout)
    )
  end

  def clear_after_timer(%__MODULE__{} = keep_alive_timer, request_ref) do
    {timeout_timer, keep_alive_timer} = pop_in(keep_alive_timer.timeout_timers[request_ref])

    cancel_timer(timeout_timer)

    keep_alive_timer
  end

  defp start_interval_timer(interval) do
    Process.send_after(self(), :keep_alive, interval)
  end

  defp cancel_timer(nil), do: :ok
  defp cancel_timer(timer), do: Process.cancel_timer(timer)
end

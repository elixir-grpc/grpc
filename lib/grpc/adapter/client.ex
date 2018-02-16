defmodule GRPC.Adapter.Client do
  def timeout(deadline, timeout) do
    cond do
      deadline -> GRPC.TimeUtils.to_relative(deadline)
      timeout -> timeout
      true -> :infinity
    end
  end
end

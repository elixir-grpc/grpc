defmodule GRPC.Adapter.Cowboy.Drainter do
  require Logger

  @doc false
  def drain(refs, opts \\ []) do
    drain_check_interval = Keyword.get(opts, :drain_check_interval, 1_000)

    refs
    |> Enum.filter(&suspend_listener/1)
    |> Enum.each(&wait_for_connections(&1, drain_check_interval))
  end

  defp suspend_listener(ref) do
    case :ranch.suspend_listener(ref) do
      :ok ->
        :ok

      other ->
        Logger.error("Got error when suspend listener #{ref}: #{inspect(other)}")
    end
  end

  defp wait_for_connections(ref, drain_check_interval) do
    if function_exported?(:ranch, :wait_for_connections, 4) do
      :ranch.wait_for_connections(ref, :==, 0, drain_check_interval)
    else
      :ranch.wait_for_connections(ref, :==, 0)
    end
  end
end

defmodule Grpc.Client.Adapters.Finch.CustomStream do
  alias GRPC.Client.Adapters.Finch.StreamState

  def start() do
    with {:ok, pid} <- StreamState.start_link(nil) do
      stream =
        Stream.resource(
          fn -> pid end,
          &next_fun/1,
          &after_fun/1
        )

      {:ok, {stream, pid}}
    end
  end

  def add_item(pid, item) do
    StreamState.add_item(pid, item)
  end

  def close(pid) do
    StreamState.close(pid)
  end

  defp next_fun(pid) do
    case StreamState.next_item(pid) do
      :close ->
        {:halt, pid}

      item ->
        {[item], pid}
    end
  end

  defp after_fun(pid) do
    IO.inspect(:close_stream, label: __MODULE__)
    GenServer.stop(pid, :normal)
  end
end

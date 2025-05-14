defmodule HelloworldStreams.Utils.Transformer do
  @moduledoc """
  `Transformer` GenServer for example purposes.
  """
  use GenServer

  alias Stream.HelloRequest
  alias Stream.HelloReply

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_), do: {:ok, %{}}

  def handle_info({:request, %HelloRequest{} = value, from}, state) do
    Process.send(from, {:response, %HelloReply{message: "Hello #{value.name}"}}, [])
    {:noreply, state}
  end
end

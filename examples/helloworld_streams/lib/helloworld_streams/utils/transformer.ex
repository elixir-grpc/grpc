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

  def handle_call({:request, %HelloRequest{} = value}, _from, state) do
    {:reply, {:response, %HelloReply{message: "Hello #{value.name}"}}, state}
  end
end

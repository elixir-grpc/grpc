defmodule GRPC.Client.LoadBalancing.Registry do
  @moduledoc false
  use GenServer

  @table :grpc_client_lb_registry

  @spec start_link(any()) :: GenServer.on_start()
  def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @spec put(reference() | term(), {module(), term()}) :: true
  def put(ref, value), do: :ets.insert(@table, {ref, value})

  @spec lookup(reference() | term()) :: {:ok, {module(), term()}} | :error
  def lookup(ref) do
    case :ets.lookup(@table, ref) do
      [{^ref, value}] -> {:ok, value}
      [] -> :error
    end
  rescue
    ArgumentError -> :error
  end

  @spec delete(reference() | term()) :: true
  def delete(ref), do: :ets.delete(@table, ref)

  @impl true
  def init(_) do
    :ets.new(@table, [:set, :public, :named_table, read_concurrency: true])
    {:ok, nil}
  end
end

defmodule GRPC.Server.Cache do
  @moduledoc """
  ETS-based cache for frequently accessed server metadata.
  Improves performance by avoiding repeated Enum.find operations.
  """

  @table_name :grpc_server_cache

  def init do
    case :ets.whereis(@table_name) do
      :undefined ->
        :ets.new(@table_name, [:named_table, :public, :set, {:read_concurrency, true}])

      _tid ->
        :ok
    end
  end

  @doc """
  Find codec by name for a given server.
  Uses ETS cache to avoid repeated Enum.find calls.
  """
  def find_codec(server, subtype) do
    cache_key = {:codec, server, subtype}

    case :ets.lookup(@table_name, cache_key) do
      [{^cache_key, codec}] ->
        codec

      [] ->
        codec = Enum.find(server.__meta__(:codecs), nil, fn c -> c.name() == subtype end)
        :ets.insert(@table_name, {cache_key, codec})
        codec
    end
  end

  @doc """
  Find compressor by name for a given server.
  Uses ETS cache to avoid repeated Enum.find calls.
  """
  def find_compressor(server, encoding) do
    cache_key = {:compressor, server, encoding}

    case :ets.lookup(@table_name, cache_key) do
      [{^cache_key, compressor}] ->
        compressor

      [] ->
        compressor =
          Enum.find(server.__meta__(:compressors), nil, fn c -> c.name() == encoding end)

        :ets.insert(@table_name, {cache_key, compressor})
        compressor
    end
  end

  @doc """
  Find RPC definition by method name for a given server.
  Uses ETS cache to avoid repeated Enum.find calls.
  """
  def find_rpc(server, method_name) do
    cache_key = {:rpc, server, method_name}

    case :ets.lookup(@table_name, cache_key) do
      [{^cache_key, rpc}] ->
        rpc

      [] ->
        rpc_calls = server.__meta__(:service).__rpc_calls__()

        rpc =
          Enum.find(rpc_calls, nil, fn {name, _, _, _} ->
            Atom.to_string(name) == method_name
          end)

        :ets.insert(@table_name, {cache_key, rpc})
        rpc
    end
  end

  @doc """
  Clear the cache. Useful for testing or when server configuration changes.
  """
  def clear do
    case :ets.whereis(@table_name) do
      :undefined -> :ok
      _tid -> :ets.delete_all_objects(@table_name)
    end
  end
end

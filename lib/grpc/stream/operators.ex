defmodule GRPC.Stream.Operators do
  @moduledoc """
  Useful and internal functions for manipulating streams.
  """
  alias GRPC.Stream, as: GRPCStream

  @type item :: any()

  @type reason :: any()

  @spec ask(GRPCStream.t(), pid | atom, non_neg_integer) ::
          GRPCStream.t() | {:error, item(), reason()}
  def ask(stream, target, timeout \\ 5000)

  def ask(%GRPCStream{flow: flow} = stream, target, timeout) when is_pid(target) do
    mapper = fn item ->
      if Process.alive?(target) do
        send(target, {:request, item, self()})

        result =
          receive do
            {:response, res} -> res
          after
            timeout -> {:error, item, :timeout}
          end

        result
      else
        {:error, item, :not_alive}
      end
    end

    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  def ask(%GRPCStream{flow: flow} = stream, target, timeout) when is_atom(target) do
    mapper = fn item ->
      if function_exported?(target, :handle_call, 3) do
        try do
          case GenServer.call(target, {:request, item}, timeout) do
            {:response, res} ->
              res

            other ->
              {:error, item,
               "Expected response from #{inspect(target)} to be in the format {:response, msg}. Found #{inspect(other)}"}
          end
        rescue
          reason ->
            {:error, item, reason}
        end
      else
        {:error, item, "#{inspect(target)} must implement the GenServer behavior"}
      end
    end

    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  @spec ask!(GRPCStream.t(), pid | atom, non_neg_integer) :: GRPCStream.t()
  def ask!(stream, target, timeout \\ 5000)

  def ask!(%GRPCStream{flow: flow} = stream, target, timeout) when is_pid(target) do
    mapper = fn item ->
      if Process.alive?(target) do
        send(target, {:request, item, self()})

        result =
          receive do
            {:response, res} -> res
          after
            timeout ->
              raise "Timeout waiting for response from #{inspect(target)}"
          end

        result
      else
        raise "Target #{inspect(target)} is not alive. Cannot send request to it."
      end
    end

    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  def ask!(%GRPCStream{flow: flow} = stream, target, timeout) when is_atom(target) do
    if not function_exported?(target, :handle_call, 3) do
      raise ArgumentError, "#{inspect(target)} must implement the GenServer behavior"
    end

    mapper = fn item ->
      case GenServer.call(target, {:request, item}, timeout) do
        {:response, res} ->
          res

        _ ->
          raise ArgumentError,
                "Expected response from #{inspect(target)} to be in the format {:response, msg}"
      end
    end

    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  @spec replace(GRPCStream.t(), (map(), term -> term)) :: GRPCStream.t()
  def replace(%GRPCStream{flow: flow, metadata: metadata} = _stream, factory)
      when is_function(factory, 2) do
    new =
      Flow.map(flow, fn item ->
        case factory.(metadata, item) do
          %GRPCStream{} = stream ->
            %GRPCStream{stream | metadata: metadata}

          _ ->
            raise ArgumentError, "Expected factory to return a GRPC.Stream"
        end
      end)
      |> Enum.to_list()
      |> Enum.at(0)

    %GRPCStream{new | flow: new.flow, metadata: new.metadata, options: new.options}
  end

  @spec filter(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def filter(%GRPCStream{flow: flow} = stream, filter) do
    %GRPCStream{stream | flow: Flow.filter(flow, filter)}
  end

  @spec flat_map(GRPCStream.t(), (term -> Enumerable.GRPCStream.t())) :: GRPCStream.t()
  def flat_map(%GRPCStream{flow: flow} = stream, flat_mapper) do
    %GRPCStream{stream | flow: Flow.flat_map(flow, flat_mapper)}
  end

  @spec map(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def map(%GRPCStream{flow: flow} = stream, mapper) do
    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  @spec map_with_ctx(GRPCStream.t(), (map(), term -> term)) :: GRPCStream.t()
  def map_with_ctx(%GRPCStream{flow: flow, metadata: meta} = stream, mapper)
      when is_function(mapper, 2) do
    wrapper = fn item ->
      mapper.(meta, item)
    end

    %GRPCStream{stream | flow: Flow.map(flow, wrapper)}
  end

  @spec partition(GRPCStream.t(), keyword()) :: GRPCStream.t()
  def partition(%GRPCStream{flow: flow} = stream, options \\ []) do
    %GRPCStream{stream | flow: Flow.partition(flow, options)}
  end

  @spec reduce(GRPCStream.t(), (-> acc), (term, acc -> acc)) :: GRPCStream.t() when acc: term()
  def reduce(%GRPCStream{flow: flow} = stream, acc_fun, reducer_fun) do
    %GRPCStream{stream | flow: Flow.reduce(flow, acc_fun, reducer_fun)}
  end

  @spec reject(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def reject(%GRPCStream{flow: flow} = stream, filter) do
    %GRPCStream{stream | flow: Flow.reject(flow, filter)}
  end

  @spec uniq(GRPCStream.t()) :: GRPCStream.t()
  def uniq(%GRPCStream{flow: flow} = stream) do
    %GRPCStream{stream | flow: Flow.uniq(flow)}
  end

  @spec uniq_by(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def uniq_by(%GRPCStream{flow: flow} = stream, fun) do
    %GRPCStream{stream | flow: Flow.uniq_by(flow, fun)}
  end
end

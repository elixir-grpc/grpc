defmodule GRPC.Stream.Operators do
  @moduledoc """
  Useful and internal functions for manipulating streams.
  """
  alias GRPC.Stream, as: GRPCStream

  @type item :: any()

  @type reason :: any()

  @spec ask(GRPCStream.t(), pid | atom, non_neg_integer) ::
          GRPCStream.t() | {:error, any(), :timeout | :not_alive}
  def ask(%GRPCStream{flow: flow} = stream, target, timeout \\ 5000) do
    mapper = fn item -> do_ask(item, target, timeout, raise_on_error: false) end
    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  @spec ask!(GRPCStream.t(), pid | atom, non_neg_integer) :: GRPCStream.t()
  def ask!(%GRPCStream{flow: flow} = stream, target, timeout \\ 5000) do
    mapper = fn item -> do_ask(item, target, timeout, raise_on_error: true) end
    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

  defp do_ask(item, target, timeout, raise_on_error: raise?) do
    resolved_target =
      case target do
        pid when is_pid(pid) -> if Process.alive?(pid), do: pid, else: nil
        atom when is_atom(atom) -> Process.whereis(atom)
      end

    cond do
      is_nil(resolved_target) and raise? ->
        raise "Target #{inspect(target)} is not alive. Cannot send request to it."

      is_nil(resolved_target) ->
        {:error, item, :not_alive}

      true ->
        send(resolved_target, {:request, item, self()})

        receive do
          {:response, res} -> res
        after
          timeout ->
            if raise? do
              raise "Timeout waiting for response from #{inspect(target)}"
            else
              {:error, item, :timeout}
            end
        end
    end
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

  @spec map_with_context(GRPCStream.t(), (map(), term -> term)) :: GRPCStream.t()
  def map_with_context(%GRPCStream{flow: flow, metadata: meta} = stream, mapper)
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

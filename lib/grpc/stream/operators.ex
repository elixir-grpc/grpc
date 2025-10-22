defmodule GRPC.Stream.Operators do
  @moduledoc """
  Useful and internal functions for manipulating streams.
  """
  alias GRPC.Stream, as: GRPCStream

  @type item :: any()

  @type reason :: any()

  @spec ask(GRPCStream.t(), pid | atom, non_neg_integer) ::
          GRPCStream.t() | {:error, :timeout | :process_not_alive}
  def ask(%GRPCStream{flow: flow} = stream, target, timeout \\ 5000) do
    # mapper = fn item -> do_ask(item, target, timeout, raise_on_error: false) end
    mapper = fn item -> safe_invoke(&do_ask(&1, target, timeout, raise_on_error: false), item) end
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
        {:error, :process_not_alive}

      true ->
        send(resolved_target, {:request, item, self()})

        receive do
          {:response, res} -> res
        after
          timeout ->
            if raise? do
              raise "Timeout waiting for response from #{inspect(target)}"
            else
              {:error, :timeout}
            end
        end
    end
  end

  @spec effect(GRPCStream.t(), (term -> term())) :: GRPCStream.t()
  def effect(%GRPCStream{flow: flow} = stream, effect_fun) when is_function(effect_fun, 1) do
    %GRPCStream{
      stream
      | flow:
          Flow.map(flow, fn flow_item ->
            tap(flow_item, fn item -> safe_invoke(effect_fun, item) end)
          end)
    }
  end

  @spec filter(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def filter(%GRPCStream{flow: flow} = stream, filter) do
    flow_wrapper = Flow.filter(flow, fn item -> safe_invoke(filter, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

  @spec flat_map(GRPCStream.t(), (term -> Enumerable.GRPCStream.t())) :: GRPCStream.t()
  def flat_map(%GRPCStream{flow: flow} = stream, flat_mapper) do
    flow_wrapper =
      Flow.flat_map(flow, fn item ->
        case safe_invoke(flat_mapper, item) do
          {:error, reason} -> [{:error, reason}]
          res -> res
        end
      end)

    %GRPCStream{stream | flow: flow_wrapper}
  end

  @spec map(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def map(%GRPCStream{flow: flow} = stream, mapper) do
    flow_wrapper = Flow.map(flow, fn item -> safe_invoke(mapper, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

  @spec map_with_context(GRPCStream.t(), (map(), term -> term)) :: GRPCStream.t()
  def map_with_context(%GRPCStream{flow: flow, metadata: meta} = stream, mapper)
      when is_function(mapper, 2) do
    wrapper = fn item ->
      mapper.(meta, item)
    end

    flow_wrapper = Flow.map(flow, fn item -> safe_invoke(wrapper, item) end)

    %GRPCStream{stream | flow: flow_wrapper}
  end

  @spec map_error(GRPCStream.t(), (reason -> term)) :: GRPCStream.t()
  def map_error(%GRPCStream{flow: flow} = stream, func) when is_function(func, 1) do
    mapper =
      Flow.map(flow, fn
        {:error, _reason} = item ->
          res = safe_invoke(func, item)

          case res do
            {:error, %GRPC.RPCError{} = new_reason} ->
              {:error, new_reason}

            {:error, new_reason} ->
              msg = "[Error] #{inspect(new_reason)}"
              {:error, GRPC.RPCError.exception(message: msg)}

            {:ok, other} ->
              other

            other ->
              other
          end

        {:ok, other} ->
          other

        other ->
          other
      end)

    %GRPCStream{stream | flow: mapper}
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
    flow_wrapper = Flow.reject(flow, fn item -> safe_invoke(filter, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

  @spec uniq(GRPCStream.t()) :: GRPCStream.t()
  def uniq(%GRPCStream{flow: flow} = stream) do
    %GRPCStream{stream | flow: Flow.uniq(flow)}
  end

  @spec uniq_by(GRPCStream.t(), (term -> term)) :: GRPCStream.t()
  def uniq_by(%GRPCStream{flow: flow} = stream, fun) do
    %GRPCStream{stream | flow: Flow.uniq_by(flow, fun)}
  end

  # Normalizes and catches exceptions/throws.
  # Returns:
  #   value -> successful value
  #   {:error, reason} -> failure
  #   {:error, {:exception, exception}} -> failure due to exception
  #   {:error, {kind, reason}} -> failure due to throw or exit
  defp safe_invoke(fun, arg) do
    res = fun.(arg)

    case res do
      {:ok, v} -> v
      {:error, reason} -> {:error, reason}
      other -> other
    end
  rescue
    e ->
      {:error, {:exception, e}}
  catch
    kind, reason ->
      {:error, {kind, reason}}
  end
end

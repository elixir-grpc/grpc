defmodule GRPC.Stream.Operators do
  @moduledoc """
  Useful and internal functions for manipulating streams.
  """
  alias GRPC.Stream, as: GRPCStream

  @type item :: any()

  @type reason :: any()

  def ask(%GRPCStream{flow: flow} = stream, target, timeout \\ 5000) do
    mapper = fn item -> safe_invoke(&do_ask(&1, target, timeout, raise_on_error: false), item) end
    %GRPCStream{stream | flow: Flow.map(flow, mapper)}
  end

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

  def effect(%GRPCStream{flow: flow} = stream, effect_fun) when is_function(effect_fun, 1) do
    flow =
      Flow.map(flow, fn flow_item ->
        tap(flow_item, fn item -> safe_invoke(effect_fun, item) end)
      end)

    %GRPCStream{stream | flow: flow}
  end

  def filter(%GRPCStream{flow: flow} = stream, filter) do
    flow_wrapper = Flow.filter(flow, fn item -> safe_invoke(filter, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

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

  def map(%GRPCStream{flow: flow} = stream, mapper) do
    flow_wrapper = Flow.map(flow, fn item -> safe_invoke(mapper, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

  def map_with_context(%GRPCStream{flow: flow, metadata: meta} = stream, mapper)
      when is_function(mapper, 2) do
    wrapper = fn item ->
      mapper.(meta, item)
    end

    flow_wrapper = Flow.map(flow, fn item -> safe_invoke(wrapper, item) end)

    %GRPCStream{stream | flow: flow_wrapper}
  end

  def map_error(%GRPCStream{flow: flow} = stream, func) when is_function(func, 1) do
    mapper =
      Flow.map(flow, fn
        {:error, reason} -> handle_error(func, reason)
        {:ok, value} -> value
        other -> other
      end)

    %GRPCStream{stream | flow: mapper}
  end

  defp handle_error(func, reason) do
    case safe_invoke(func, {:error, reason}) do
      {:error, %GRPC.RPCError{} = rpc_error} ->
        {:error, rpc_error}

      {:error, other_reason} ->
        {:error, GRPC.RPCError.exception(message: inspect(other_reason))}

      {:ok, value} ->
        value

      other ->
        other
    end
  end

  def partition(%GRPCStream{flow: flow} = stream, options \\ []) do
    %GRPCStream{stream | flow: Flow.partition(flow, options)}
  end

  def reduce(%GRPCStream{flow: flow} = stream, acc_fun, reducer_fun) do
    %GRPCStream{stream | flow: Flow.reduce(flow, acc_fun, reducer_fun)}
  end

  def reject(%GRPCStream{flow: flow} = stream, filter) do
    flow_wrapper = Flow.reject(flow, fn item -> safe_invoke(filter, item) end)
    %GRPCStream{stream | flow: flow_wrapper}
  end

  def uniq(%GRPCStream{flow: flow} = stream) do
    %GRPCStream{stream | flow: Flow.uniq(flow)}
  end

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

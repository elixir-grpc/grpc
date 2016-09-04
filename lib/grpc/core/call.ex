defmodule GRPC.Core.Call do
  import GRPC.Nifs

  def create(channel, parent_call, propagation_mask, completion_queue, method, host, deadline) do
    call_create(channel, parent_call, propagation_mask, completion_queue, method, host, deadline)
  end

  def run_batch(call, ops, tag) when is_map(ops) do
    call_run_batch(call, ops, tag)
  end

  def finish_batch(stack, completion_queue, tag, timeout) do
    call_finish_batch(stack, completion_queue, tag, timeout)
  end
end

defmodule GRPC.Core.Call do
  import GRPC.Nifs

  def create(channel, parent_call, propagation_mask, completion_queue, method, host, deadline) do
    call_create(channel, parent_call, propagation_mask, completion_queue, method, host, deadline)
  end
end

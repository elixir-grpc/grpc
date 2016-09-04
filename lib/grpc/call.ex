defmodule GRPC.Call do
  def unary(call, cq, message, metadata) do
    ops = %{
      op(:SEND_INITIAL_METADATA) => metadata,
      op(:SEND_MESSAGE) => message,
      op(:SEND_CLOSE_FROM_CLIENT) => true,
      op(:RECV_INITIAL_METADATA) => true,
      op(:RECV_MESSAGE) => true,
      op(:RECV_STATUS_ON_CLIENT) => true
    }
    stack = GRPC.Core.Call.run_batch(call, ops, nil)
    GRPC.Core.Call.finish_batch(stack, cq, nil, 5)
  end

  # http://www.grpc.io/grpc/core/grpc__types_8h.html#a41dc9abf9678decbfcef1a037e35939a
  def op(atom) when is_atom(atom) do
    ops = ~w(
      GRPC_OP_SEND_INITIAL_METADATA
      GRPC_OP_SEND_MESSAGE
      GRPC_OP_SEND_CLOSE_FROM_CLIENT
      GRPC_OP_SEND_STATUS_FROM_SERVER
      GRPC_OP_RECV_INITIAL_METADATA
      GRPC_OP_RECV_MESSAGE
      GRPC_OP_RECV_STATUS_ON_CLIENT
      GRPC_OP_RECV_CLOSE_ON_SERVER
    )
    index = Enum.find_index(ops, fn(x) -> to_string(atom) == String.replace_prefix(x, "GRPC_OP_", "") end)
    if !index, do: raise(ArgumentError, message: "#{atom} is invalid!")
    index
  end
end

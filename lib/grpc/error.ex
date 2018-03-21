defmodule GRPC.RPCError do
  @moduledoc """
  The RPC error raised in server side and got in client side.

      # server side
      raise GRPC.RPCError, status: GRPC.Status.unknown, message: "error message"

      # client side
      {:error, error} = Your.Stub.unary_call(channel, request)
  """

  defexception [:status, :message]
  @type t :: %__MODULE__{status: GRPC.Status.t(), message: String.t()}

  @spec exception(GRPC.Status.t(), String.t()) :: t
  def exception(status, message) do
    %GRPC.RPCError{status: status, message: message}
  end
end

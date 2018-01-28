defmodule GRPC.RPCError do
  defexception [:status, :message]

  def exception(status, message) do
    %GRPC.RPCError{status: status, message: message}
  end
end

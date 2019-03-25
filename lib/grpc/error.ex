defmodule GRPC.RPCError do
  @moduledoc """
  The RPC error raised in server side and got in client side.

      # server side
      raise GRPC.RPCError, status: :unknown # preferred
      raise GRPC.RPCError, status: GRPC.Status.unknown, message: "error message"

      # client side
      {:error, error} = Your.Stub.unary_call(channel, request)
  """

  defexception [:status, :message]
  @type t :: %__MODULE__{status: GRPC.Status.t(), message: String.t()}

  alias GRPC.Status

  @spec exception(Status.t(), String.t()) :: t
  def new(status) when is_atom(status) do
    exception(status: status)
  end

  def exception(args) when is_list(args) do
    error = parse_args(args, %__MODULE__{})

    if error.message do
      error
    else
      Map.put(error, :message, status_message(error.status))
    end
  end

  defp parse_args([], acc), do: acc

  defp parse_args([{:status, status} | t], acc) when is_integer(status) do
    acc = %{acc | status: status}
    parse_args(t, acc)
  end

  defp parse_args([{:status, status} | t], acc) when is_atom(status) do
    acc = %{acc | status: apply(GRPC.Status, status, [])}
    parse_args(t, acc)
  end

  defp parse_args([{:message, message} | t], acc) when is_binary(message) do
    acc = %{acc | message: message}
    parse_args(t, acc)
  end

  def exception(status, message) when is_atom(status) do
    %GRPC.RPCError{status: apply(GRPC.Status, status, []), message: message}
  end

  def exception(status, message) when is_integer(status) do
    %GRPC.RPCError{status: status, message: message}
  end

  defp status_message(1), do: "The operation was cancelled (typically by the caller)"
  defp status_message(2), do: "Unknown error"
  defp status_message(3), do: "Client specified an invalid argument"
  defp status_message(4), do: "Deadline expired before operation could complete"
  defp status_message(5), do: "Some requested entity (e.g., file or directory) was not found"

  defp status_message(6),
    do: "Some entity that we attempted to create (e.g., file or directory) already exists"

  defp status_message(7),
    do: "The caller does not have permission to execute the specified operation"

  defp status_message(8), do: "Some resource has been exhausted"

  defp status_message(9),
    do:
      "Operation was rejected because the system is not in a state required for the operation's execution"

  defp status_message(10), do: "The operation was aborted"
  defp status_message(11), do: "Operation was attempted past the valid range"

  defp status_message(12),
    do: "Operation is not implemented or not supported/enabled in this service"

  defp status_message(13), do: "Internal errors"
  defp status_message(14), do: "The service is currently unavailable"
  defp status_message(15), do: "Unrecoverable data loss or corruption"

  defp status_message(16),
    do: "The request does not have valid authentication credentials for the operation"
end

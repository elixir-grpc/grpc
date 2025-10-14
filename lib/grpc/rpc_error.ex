defmodule GRPC.RPCError do
  @moduledoc """
  The RPC error raised in server side and got in client side.

      # server side
      raise GRPC.RPCError, status: :unknown # preferred
      raise GRPC.RPCError, status: GRPC.Status.unknown, message: "error message"

      # client side
      {:error, error} = Your.Stub.unary_call(channel, request)

  Error handling can be done with the `is_rpc_error/2` guard.

  Expanding on the code above, the first option is for the guard to
  be used in a `cond` or `case`, as follows:

      cond do
        is_rpc_error(error, GRPC.Status.not_found()) ->
          do_something_when_not_found()

        is_rpc_error(error, GRPC.Status.out_of_range()) ->
          do_something_when_out_of_range()

        true ->
          fallback_code()
      end

  Another option is for the error handling to be written into a multi-clause function.
  In such case we must define module attributes for each of the errors we want because
  the functions in `GRPC.Status` can't be called directly inside the guard.

      ...
      handle_error(error)
      ...

      @not_found GRPC.Status.not_found()
      @out_of_range GRPC.Status.out_of_range()

      defp handle_error(error) when is_rpc_error(error, @not_found) do
        # not found
      end

      defp handle_error(error) when is_rpc_error(error, @out_of_range) do
        # out of range
      end

      defp handle_error(error) do
        # fallback
      end

  See `GRPC.Status` for more details on possible statuses.
  """

  defexception [:status, :message, :details]

  defguard is_rpc_error(e, status) when is_struct(e, __MODULE__) and e.status == status

  @type t :: %__MODULE__{
          status: GRPC.Status.t(),
          message: String.t(),
          details: [Google.Protobuf.Any.t()]
        }

  alias GRPC.Status

  @spec new(status :: atom()) :: t()
  def new(status) when is_atom(status) do
    exception(status: status)
  end

  @spec exception(args :: list()) :: t()
  def exception(args) when is_list(args) do
    error = parse_args(args, %__MODULE__{})

    %{
      error
      | message: error.message || Status.status_message(error.status),
        details: error.details
    }
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

  defp parse_args([{:details, details} | t], acc) when is_list(details) do
    acc = %{acc | details: details}
    parse_args(t, acc)
  end

  @spec exception(status :: Status.t() | atom(), message :: String.t()) :: t()
  def exception(status, message) when is_atom(status) do
    %GRPC.RPCError{status: apply(GRPC.Status, status, []), message: message}
  end

  def exception(status, message) when is_integer(status) do
    %GRPC.RPCError{status: status, message: message}
  end

  @doc false
  def from_grpc_status_details_bin(%{
        status: status,
        message: message,
        encoded_details_bin: encoded_details_bin
      })
      when is_binary(encoded_details_bin) do
    case GRPC.Google.RPC.decode_status(encoded_details_bin) do
      {:ok, rpc_status} ->
        %__MODULE__{
          status: status,
          message: rpc_status.message,
          details: rpc_status.details
        }

      {:error, _} ->
        %__MODULE__{status: status, message: message}
    end
  end

  def from_grpc_status_details_bin(%{status: status, message: message}) do
    %__MODULE__{status: status, message: message}
  end
end

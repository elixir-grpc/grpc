defmodule GRPC.Client.MessageLimit do
  @moduledoc false
  # The receive-side size check for client adapters. It reads the length prefix of
  # a message before the message is sliced out of the buffer, so an oversize message
  # fails with RESOURCE_EXHAUSTED without being read. `GRPC.Message` in grpc_core
  # does the same check in newer releases; this module keeps the client correct
  # against the grpc_core release that `grpc` requires.

  import Bitwise

  # Override with: config :grpc, max_message_length: bytes
  @default_max_message_length bsl(1, 32 - 1)

  @doc "The `:max_message_length` setting, in bytes."
  def max_message_length do
    Application.get_env(:grpc, :max_message_length, @default_max_message_length)
  end

  @doc """
  Checks the length in the prefix of the first message in `data`.

  Returns `:ok` when it is within the limit, or when `data` has no full prefix yet.
  """
  @spec check_prefix(binary) :: :ok | {:error, Exception.t()}
  def check_prefix(<<_flag::8, length::big-32, _::binary>>) do
    max_length = max_message_length()

    if length > max_length do
      {:error,
       GRPC.RPCError.exception(
         status: :resource_exhausted,
         message:
           "Received message is too large (#{length} bytes, the limit is #{max_length} bytes)"
       )}
    else
      :ok
    end
  end

  def check_prefix(_data), do: :ok
end

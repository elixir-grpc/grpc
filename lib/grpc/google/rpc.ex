defmodule GRPC.Google.RPC do
  @moduledoc false

  @spec encode_status(Google.Rpc.Status.t()) :: String.t()
  def encode_status(%Google.Rpc.Status{} = status) do
    status
    |> Google.Rpc.Status.encode()
    |> Base.encode64(padding: true)
  end

  @spec decode_status(String.t()) :: {:ok, Google.Rpc.Status.t()} | {:error, term()}
  def decode_status(encoded_details_bin) when is_binary(encoded_details_bin) do
    {:ok,
     encoded_details_bin
     |> decode64()
     |> Google.Rpc.Status.decode()}
  rescue
    e -> {:error, e}
  end

  defp decode64(str) when rem(byte_size(str), 4) == 0 do
    Base.decode64!(str, padding: true)
  end

  defp decode64(str) do
    Base.decode64!(str, padding: false)
  end
end

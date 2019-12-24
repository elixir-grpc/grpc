defmodule GRPC.Transport.Utils do
  @moduledoc false

  # unit: ns
  @ns_ceiling 1000
  @us_ceiling 1000_000
  # @ms_ceiling @us_ceiling * 1000

  # unit: ms
  @ms_ceiling 1000
  @second_ceiling @ms_ceiling * 60
  @minute_ceiling @second_ceiling * 60

  @doc """
  Encode timeout(nanoseconds) by gRPC protocol
  """
  def encode_ns(timeout) when timeout <= 0, do: "0u"

  def encode_ns(timeout) when is_integer(timeout) and timeout < @ns_ceiling do
    to_string(timeout) <> "n"
  end

  def encode_ns(timeout) when timeout < @us_ceiling do
    to_string(div(timeout, 1000)) <> "u"
  end

  @doc """
  Encode timeout(milliseconds) by gRPC protocol
  """
  def encode_timeout(timeout) when timeout <= 0, do: "0u"

  def encode_timeout(timeout) when is_integer(timeout) and timeout < @ms_ceiling do
    to_string(timeout) <> "m"
  end

  def encode_timeout(timeout) when timeout < @second_ceiling do
    to_string(div(timeout, 1000)) <> "S"
  end

  def encode_timeout(timeout) when timeout < @minute_ceiling do
    to_string(div(timeout, 1000 * 60)) <> "M"
  end

  def encode_timeout(timeout) do
    to_string(div(timeout, 1000 * 3600)) <> "H"
  end

  @spec decode_timeout(String.t()) :: non_neg_integer()
  def decode_timeout(timeout) do
    {timeout, unit} = String.split_at(timeout, -1)
    decode_timeout(unit, String.to_integer(timeout))
  end

  defp decode_timeout("n", timeout) do
    div(timeout, 1000_000)
  end

  defp decode_timeout("u", timeout) do
    div(timeout, 1000)
  end

  defp decode_timeout("m", timeout) do
    timeout
  end

  defp decode_timeout("S", timeout) do
    timeout * 1000
  end

  defp decode_timeout("M", timeout) do
    timeout * 60_000
  end

  defp decode_timeout("H", timeout) do
    timeout * 3_600_000
  end
end

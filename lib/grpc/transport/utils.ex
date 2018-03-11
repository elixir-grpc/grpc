defmodule GRPC.Transport.Utils do
  @us_ceiling 100_000_000
  @ms_ceiling @us_ceiling * 1000
  @second_ceiling @ms_ceiling * 1000
  @minute_ceiling @second_ceiling * 60
  @hour_ceiling @minute_ceiling * 60

  @doc """
  Encode deadline by gRPC guide
  """
  def encode_timeout(timeout) when timeout <= 0, do: "0u"

  def encode_timeout(timeout) when timeout < @us_ceiling do
    to_string(timeout) <> "u"
  end

  def encode_timeout(timeout) when timeout < @ms_ceiling do
    to_string(div(timeout, 1000)) <> "m"
  end

  def encode_timeout(timeout) when timeout < @second_ceiling do
    to_string(div(timeout, 1000_000)) <> "S"
  end

  def encode_timeout(timeout) when timeout < @minute_ceiling do
    to_string(div(timeout, 1000_000 * 60)) <> "M"
  end

  def encode_timeout(timeout) when timeout < @hour_ceiling do
    to_string(div(timeout, 1000_000 * 3600)) <> "H"
  end

  def encode_timeout(_timeout) do
    to_string(@us_ceiling - 1) <> "H"
  end
end

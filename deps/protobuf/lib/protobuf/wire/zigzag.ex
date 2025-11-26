defmodule Protobuf.Wire.Zigzag do
  @moduledoc false

  import Bitwise

  @spec encode(integer) :: integer
  def encode(n) when n >= 0, do: n * 2
  def encode(n) when n < 0, do: n * -2 - 1

  @spec decode(integer) :: integer
  def decode(n) when band(n, 1) == 0, do: bsr(n, 1)
  def decode(n) when band(n, 1) == 1, do: -bsr(n + 1, 1)
end

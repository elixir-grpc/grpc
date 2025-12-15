defmodule GRPC.Transport.HTTP2.Frame.Flags do
  @moduledoc false
  import Bitwise

  defguard set?(flags, bit) when band(flags, bsl(1, bit)) != 0

  @spec set(list(0..7)) :: 0..255
  def set(bits) do
    Enum.reduce(bits, 0, fn bit, acc -> bor(acc, bsl(1, bit)) end)
  end
end

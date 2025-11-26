defmodule Protobuf.Wire.Varint do
  @moduledoc false

  # Varint encoding and decoding utilities.
  #
  # https://developers.google.com/protocol-buffers/docs/encoding#varints
  #
  # For performance reasons, varint decoding must be built through a macro, so that binary
  # match contexts are reused and no new large binaries get allocated. You can define your
  # own varint decoders with the `decoder` macro, which generates function heads for up to
  # 10-bytes long varint-encoded data.
  #
  #     defmodule VarintDecoders do
  #       import Protobuf.Wire.Varint
  #
  #       decoder :def, :decode_and_sum, [:plus] do
  #         {:ok, value + plus, rest}
  #       end
  #
  #       def decode_all(<<bin::bits>>), do: decode_all(bin, [])
  #
  #       defp decode_all(<<>>, acc), do: acc
  #
  #       defdecoderp decode_all(acc) do
  #         decode_all(rest, [value | acc])
  #       end
  #     end
  #
  #     iex> VarintDecoders.decode_and_sum(<<35>>, 7)
  #     {:ok, 42, ""}
  #
  #     iex> VarintDecoders.decode_all("abcd asdf")
  #     [102, 100, 115, 97, 32, 100, 99, 98, 97]
  #
  # Refer to [efficiency guide](http://www1.erlang.org/doc/efficiency_guide/binaryhandling.html)
  # for more on efficient binary handling.
  #
  # Encoding on the other hand is simpler. It takes an integer and returns an iolist with its
  # varint representation:
  #
  #     iex> Protobuf.Wire.Varint.encode(35)
  #     [35]
  #
  #     iex> Protobuf.Wire.Varint.encode(1_234_567)
  #     [<<135>>, <<173>>, 75]

  import Bitwise

  @max_bits 64
  @mask64 bsl(1, @max_bits) - 1

  # generated: true is required here to silence compilation warnings in Elixir
  # 1.10 and 1.11. OK to remove once we support only 1.12+
  @varints [
    {
      quote(do: <<0::1, value::7>>),
      quote(do: value)
    },
    {
      quote(do: <<1::1, x0::7, 0::1, x1::7>>),
      quote(generated: true, do: x0 + bsl(x1, 7))
    },
    {
      quote(do: <<1::1, x0::7, 1::1, x1::7, 0::1, x2::7>>),
      quote(generated: true, do: x0 + bsl(x1, 7) + bsl(x2, 14))
    },
    {
      quote(do: <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 0::1, x3::7>>),
      quote(generated: true, do: x0 + bsl(x1, 7) + bsl(x2, 14) + bsl(x3, 21))
    },
    {
      quote(do: <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 0::1, x4::7>>),
      quote(generated: true, do: x0 + bsl(x1, 7) + bsl(x2, 14) + bsl(x3, 21) + bsl(x4, 28))
    },
    {
      quote do
        <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 1::1, x4::7, 0::1, x5::7>>
      end,
      quote(generated: true) do
        x0 +
          bsl(x1, 7) +
          bsl(x2, 14) +
          bsl(x3, 21) +
          bsl(x4, 28) +
          bsl(x5, 35)
      end
    },
    {
      quote do
        <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 1::1, x4::7, 1::1, x5::7, 0::1,
          x6::7>>
      end,
      quote(generated: true) do
        x0 +
          bsl(x1, 7) +
          bsl(x2, 14) +
          bsl(x3, 21) +
          bsl(x4, 28) +
          bsl(x5, 35) +
          bsl(x6, 42)
      end
    },
    {
      quote do
        <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 1::1, x4::7, 1::1, x5::7, 1::1,
          x6::7, 0::1, x7::7>>
      end,
      quote(generated: true) do
        x0 +
          bsl(x1, 7) +
          bsl(x2, 14) +
          bsl(x3, 21) +
          bsl(x4, 28) +
          bsl(x5, 35) +
          bsl(x6, 42) +
          bsl(x7, 49)
      end
    },
    {
      quote do
        <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 1::1, x4::7, 1::1, x5::7, 1::1,
          x6::7, 1::1, x7::7, 0::1, x8::7>>
      end,
      quote(generated: true) do
        x0 +
          bsl(x1, 7) +
          bsl(x2, 14) +
          bsl(x3, 21) +
          bsl(x4, 28) +
          bsl(x5, 35) +
          bsl(x6, 42) +
          bsl(x7, 49) +
          bsl(x8, 56)
      end
    },
    {
      quote do
        <<1::1, x0::7, 1::1, x1::7, 1::1, x2::7, 1::1, x3::7, 1::1, x4::7, 1::1, x5::7, 1::1,
          x6::7, 1::1, x7::7, 1::1, x8::7, 0::1, x9::7>>
      end,
      quote(generated: true) do
        v =
          x0 +
            bsl(x1, 7) +
            bsl(x2, 14) +
            bsl(x3, 21) +
            bsl(x4, 28) +
            bsl(x5, 35) +
            bsl(x6, 42) +
            bsl(x7, 49) +
            bsl(x8, 56) +
            bsl(x9, 63)

        _ = band(v, unquote(@mask64))
      end
    }
  ]

  defmacro defdecoderp(name_and_args, do: body) do
    {name, args} = Macro.decompose_call(name_and_args)

    def_decoder_success_clauses(name, args, body) ++ [def_decoder_failure_clause(name, args)]
  end

  defp def_decoder_success_clauses(name, args, body) do
    for {pattern, expression} <- @varints do
      quote do
        defp unquote(name)(<<unquote(pattern), rest::bits>>, unquote_splicing(args)) do
          var!(value) = unquote(expression)
          var!(rest) = rest
          unquote(body)
        end
      end
    end
  end

  defp def_decoder_failure_clause(name, args) do
    args =
      Enum.map(args, fn
        {:_, _meta, _ctxt} = underscore -> underscore
        {name, meta, ctxt} when is_atom(name) and is_atom(ctxt) -> {:"_#{name}", meta, ctxt}
        other -> other
      end)

    quote do
      defp unquote(name)(<<_::bits>>, unquote_splicing(args)) do
        raise Protobuf.DecodeError, message: "cannot decode binary data"
      end
    end
  end

  @spec encode(integer) :: iolist
  def encode(n) when n < 0 do
    <<n::64-unsigned-native>> = <<n::64-signed-native>>
    encode(n)
  end

  def encode(n) when n <= 127 do
    [n]
  end

  def encode(n) do
    [<<1::1, band(n, 127)::7>> | encode(bsr(n, 7))]
  end
end

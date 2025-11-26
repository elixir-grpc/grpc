defmodule Protobuf.Wire do
  @moduledoc false

  import Protobuf.Wire.Types

  alias Protobuf.Wire.{Varint, Zigzag}

  @compile {:inline, encode_from_wire_type: 2}

  @type proto_type() ::
          :int32
          | :int64
          | :fixed32
          | :fixed64
          | :uint32
          | :uint64
          | :sfixed32
          | :sfixed64
          | :sint32
          | :sint64
          | :float
          | :double
          | :bool
          | :string
          | :bytes
          | {:enum, module() | integer()}

  @type proto_float() :: :infinity | :negative_infinity | :nan | float()

  @type proto_value() :: binary() | integer() | boolean() | proto_float() | atom()

  @sint32_range -0x80000000..0x7FFFFFFF
  @sint64_range -0x8000000000000000..0x7FFFFFFFFFFFFFFF
  @uint32_range 0..0xFFFFFFFF
  @uint64_range 0..0xFFFFFFFFFFFFFFFF

  @spec wire_type(proto_type() | module()) :: Protobuf.wire_type()
  def wire_type(:int32), do: wire_varint()
  def wire_type(:int64), do: wire_varint()
  def wire_type(:uint32), do: wire_varint()
  def wire_type(:uint64), do: wire_varint()
  def wire_type(:sint32), do: wire_varint()
  def wire_type(:sint64), do: wire_varint()
  def wire_type(:bool), do: wire_varint()
  def wire_type({:enum, _enum_module}), do: wire_varint()
  def wire_type(:fixed64), do: wire_64bits()
  def wire_type(:sfixed64), do: wire_64bits()
  def wire_type(:double), do: wire_64bits()
  def wire_type(:string), do: wire_delimited()
  def wire_type(:bytes), do: wire_delimited()
  def wire_type(:fixed32), do: wire_32bits()
  def wire_type(:sfixed32), do: wire_32bits()
  def wire_type(:float), do: wire_32bits()
  def wire_type(mod) when is_atom(mod), do: wire_delimited()

  @spec encode_from_wire_type(Protobuf.wire_type(), term()) :: iodata()
  def encode_from_wire_type(wire_type, value)

  def encode_from_wire_type(wire_varint(), int) when is_integer(int), do: Varint.encode(int)

  # Returns improper list, but still valid iodata.
  def encode_from_wire_type(wire_delimited(), bin) when is_binary(bin),
    do: [Varint.encode(byte_size(bin)) | bin]

  def encode_from_wire_type(wire_64bits(), bin) when is_binary(bin) and bit_size(bin) == 64,
    do: bin

  def encode_from_wire_type(wire_32bits(), bin) when is_binary(bin) and bit_size(bin) == 32,
    do: bin

  @spec encode(proto_type(), proto_value()) :: iodata()
  def encode(type, value)

  def encode(:string, binary) when is_binary(binary) do
    unless String.valid?(binary) do
      raise Protobuf.EncodeError,
        message: "invalid UTF-8 data for type string: #{inspect(binary)}"
    end

    encode_from_wire_type(wire_delimited(), binary)
  end

  def encode(:bytes, binary) when is_binary(binary),
    do: encode_from_wire_type(wire_delimited(), binary)

  def encode(:int32, n) when n in @sint32_range, do: encode_from_wire_type(wire_varint(), n)
  def encode(:int64, n) when n in @sint64_range, do: encode_from_wire_type(wire_varint(), n)
  def encode(:uint32, n) when n in @uint32_range, do: encode_from_wire_type(wire_varint(), n)
  def encode(:uint64, n) when n in @uint64_range, do: encode_from_wire_type(wire_varint(), n)

  def encode(:bool, true), do: encode_from_wire_type(wire_varint(), 1)
  def encode(:bool, false), do: encode_from_wire_type(wire_varint(), 0)

  def encode({:enum, enum_mod}, key) when is_atom(enum_mod) and is_atom(key),
    do: encode_from_wire_type(wire_varint(), enum_mod.value(key))

  def encode({:enum, enum_mod}, number) when is_atom(enum_mod) and is_integer(number),
    do: encode_from_wire_type(wire_varint(), number)

  def encode(:float, :infinity), do: encode_from_wire_type(wire_32bits(), <<0, 0, 128, 127>>)

  def encode(:float, :negative_infinity),
    do: encode_from_wire_type(wire_32bits(), <<0, 0, 128, 255>>)

  def encode(:float, :nan), do: encode_from_wire_type(wire_32bits(), <<0, 0, 192, 127>>)
  def encode(:float, n), do: encode_from_wire_type(wire_32bits(), <<n::32-float-little>>)

  def encode(:double, :infinity),
    do: encode_from_wire_type(wire_64bits(), <<0, 0, 0, 0, 0, 0, 240, 127>>)

  def encode(:double, :negative_infinity),
    do: encode_from_wire_type(wire_64bits(), <<0, 0, 0, 0, 0, 0, 240, 255>>)

  def encode(:double, :nan),
    do: encode_from_wire_type(wire_64bits(), <<1, 0, 0, 0, 0, 0, 248, 127>>)

  def encode(:double, n), do: encode_from_wire_type(wire_64bits(), <<n::64-float-little>>)

  def encode(:sint32, n) when n in @sint32_range,
    do: encode_from_wire_type(wire_varint(), Zigzag.encode(n))

  def encode(:sint64, n) when n in @sint64_range,
    do: encode_from_wire_type(wire_varint(), Zigzag.encode(n))

  def encode(:fixed32, n) when n in @uint32_range,
    do: encode_from_wire_type(wire_32bits(), <<n::32-little>>)

  def encode(:fixed64, n) when n in @uint64_range,
    do: encode_from_wire_type(wire_64bits(), <<n::64-little>>)

  def encode(:sfixed32, n) when n in @sint32_range,
    do: encode_from_wire_type(wire_32bits(), <<n::32-signed-little>>)

  def encode(:sfixed64, n) when n in @sint64_range,
    do: encode_from_wire_type(wire_64bits(), <<n::64-signed-little>>)

  def encode(type, value) do
    raise Protobuf.EncodeError,
      message: "#{inspect(value)} is invalid for type #{inspect(type)}"
  end

  @spec decode(proto_type(), binary() | integer()) :: proto_value()
  def decode(type, value)

  def decode(:string, binary) when is_binary(binary) do
    unless String.valid?(binary) do
      raise Protobuf.DecodeError,
        message: "invalid UTF-8 data for type string: #{inspect(binary)}"
    end

    binary
  end

  def decode(:bytes, binary) when is_binary(binary), do: binary

  def decode(:int32, val) do
    <<n::signed-integer-32>> = <<val::32>>
    n
  end

  def decode(:int64, val) do
    <<n::signed-integer-64>> = <<val::64>>
    n
  end

  def decode(:uint32, val) do
    <<n::unsigned-integer-32>> = <<val::32>>
    n
  end

  def decode(:uint64, val) do
    <<n::unsigned-integer-64>> = <<val::64>>
    n
  end

  def decode(:bool, val), do: val != 0

  def decode({:enum, enum_mod}, val) when is_atom(enum_mod) and is_integer(val) do
    # We cast the integer to int32 first.
    number = decode(:int32, val)
    # key/1 returns the integer "number" itself if no atom key is found for "number".
    enum_mod.key(number)
  end

  def decode(:float, <<n::little-float-32>>), do: n
  # little endianness, should be 0b0_11111111_000000000...
  def decode(:float, <<0, 0, 0b1000_0000::8, 0b01111111::8>>), do: :infinity
  # little endianness, should be 0b1_11111111_000000000...
  def decode(:float, <<0, 0, 0b1000_0000::8, 0b11111111::8>>), do: :negative_infinity
  # should be 0b*_11111111_not_zero...
  def decode(:float, <<a::16, 1::1, b::7, _::1, 0b1111111::7>>) when a != 0 or b != 0,
    do: :nan

  def decode(:double, <<n::little-float-64>>), do: n
  # little endianness, should be 0b0_11111111111_000000000...
  def decode(:double, <<0::48, 0b1111::4, 0::4, 0b01111111::8>>), do: :infinity
  # little endianness, should be 0b1_11111111111_000000000...
  def decode(:double, <<0::48, 0b1111::4, 0::4, 0b11111111::8>>), do: :negative_infinity

  def decode(:double, <<a::48, 0b1111::4, b::4, _::1, 0b1111111::7>>) when a != 0 or b != 0,
    do: :nan

  # For sint32 and sint64, we cast to their respective uint first and only then use zigzag
  # decoding. This ensures that if, for example, a 32 bit integer overflows because it was encoded
  # as a 64 bit integer (which is allowed for forward and backward compatibility), we first cast
  # it to a 32 bit integer and then zigzag-decode it.
  def decode(:sint32, val), do: Zigzag.decode(decode(:uint32, val))
  def decode(:sint64, val), do: Zigzag.decode(decode(:uint64, val))

  def decode(:fixed32, <<n::little-32>>), do: n
  def decode(:fixed64, <<n::little-64>>), do: n
  def decode(:sfixed32, <<n::little-signed-32>>), do: n
  def decode(:sfixed64, <<n::little-signed-64>>), do: n

  def decode(type, val) do
    raise Protobuf.DecodeError, message: "can't decode #{inspect(val)} into type #{inspect(type)}"
  end
end

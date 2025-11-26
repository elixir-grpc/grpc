defmodule Protobuf.Decoder do
  @moduledoc false

  import Bitwise, only: [bsr: 2, band: 2]
  import Protobuf.{Wire.Types, Wire.Varint}

  alias Protobuf.{DecodeError, FieldProps, MessageProps, Wire}

  @compile {:inline,
            decode_field: 3, skip_varint: 4, skip_delimited: 4, reverse_repeated: 2, field_key: 2}

  @spec decode(binary(), module()) :: term()
  def decode(bin, module) when is_binary(bin) and is_atom(module) do
    %MessageProps{repeated_fields: repeated_fields} = props = module.__message_props__()

    bin
    |> build_message(struct(module), props)
    |> reverse_repeated(repeated_fields)
    |> Map.update!(:__unknown_fields__, &Enum.reverse/1)
    |> transform_module(module)
  end

  defp transform_module(message, module) do
    if transform_module = module.transform_module() do
      transform_module.decode(message, module)
    else
      message
    end
  end

  defp build_message(<<>>, message, _props), do: message

  defp build_message(<<bin::bits>>, message, props) do
    decode_field(bin, message, props)
  end

  defdecoderp decode_field(message, props) do
    # From the docs:
    # "Each key in the streamed message is a varint with the value
    # (field_number << 3) | wire_type, in other words, the last three bits of
    # the number store the wire type."
    field_number = bsr(value, 3)
    wire_type = band(value, 0b00000111)

    if field_number != 0 do
      handle_field(rest, field_number, wire_type, message, props)
    else
      raise Protobuf.DecodeError, message: "invalid field number 0 when decoding binary data"
    end
  end

  defp handle_field(<<bin::bits>>, field_number, wire_start_group(), message, props) do
    skip_field(bin, message, props, [field_number])
  end

  defp handle_field(<<_bin::bits>>, closing, wire_end_group(), _message, _props) do
    msg = "closing group #{inspect(closing)} but no groups are open"
    raise Protobuf.DecodeError, message: msg
  end

  defp handle_field(<<bin::bits>>, field_number, wire_varint(), message, props) do
    decode_varint(bin, field_number, message, props)
  end

  defp handle_field(<<bin::bits>>, field_number, wire_delimited(), message, props) do
    decode_delimited(bin, field_number, message, props)
  end

  defp handle_field(<<bin::bits>>, field_number, wire_32bits(), message, props) do
    <<value::bits-32, rest::bits>> = bin
    handle_value(rest, field_number, wire_32bits(), value, message, props)
  end

  defp handle_field(<<bin::bits>>, field_number, wire_64bits(), message, props) do
    <<value::bits-64, rest::bits>> = bin
    handle_value(rest, field_number, wire_64bits(), value, message, props)
  end

  defp handle_field(_bin, _field_number, wire_type, _message, _props) do
    raise Protobuf.DecodeError,
      message: "cannot decode binary data, unknown wire type: #{inspect(wire_type)}"
  end

  defdecoderp skip_field(message, props, groups) do
    field_number = bsr(value, 3)
    wire_type = band(value, 7)

    case wire_type do
      wire_start_group() ->
        skip_field(rest, message, props, [field_number | groups])

      wire_end_group() ->
        case groups do
          [^field_number] ->
            build_message(rest, message, props)

          [^field_number | groups] ->
            skip_field(rest, message, props, groups)

          [group | _] ->
            msg = "closing group #{inspect(field_number)} but group #{inspect(group)} is open"
            raise Protobuf.DecodeError, message: msg
        end

      wire_varint() ->
        skip_varint(rest, message, props, groups)

      wire_delimited() ->
        skip_delimited(rest, message, props, groups)

      wire_32bits() ->
        rest |> skip_bits(32) |> skip_field(message, props, groups)

      wire_64bits() ->
        rest |> skip_bits(64) |> skip_field(message, props, groups)

      wire_type ->
        message =
          case props.field_props do
            %{^field_number => %FieldProps{wire_type: expected, name: field}} ->
              "field #{field}: got #{wire_type}, expected #{expected}"

            _ ->
              "field_number #{field_number}: got #{wire_type}"
          end

        raise DecodeError, message: "invalid wire_type for skipped " <> message
    end
  end

  defdecoderp skip_varint(message, props, groups) do
    _ = value
    skip_field(rest, message, props, groups)
  end

  defdecoderp skip_delimited(message, props, groups) do
    <<_skip::bytes-size(value), rest::bits>> = rest
    skip_field(rest, message, props, groups)
  end

  defp skip_bits(binary, length) do
    case binary do
      <<_::bits-size(length), rest::bits>> -> rest
      _ -> raise DecodeError, message: "insufficient data for skipping #{length} bits"
    end
  end

  defdecoderp decode_varint(field_number, message, props) do
    handle_value(rest, field_number, wire_varint(), value, message, props)
  end

  defdecoderp decode_delimited(field_number, message, props) do
    bytes_remaining = byte_size(rest)

    if value <= bytes_remaining do
      <<bytes::bytes-size(value), rest::bits>> = rest
      handle_value(rest, field_number, wire_delimited(), bytes, message, props)
    else
      field =
        case props.field_props do
          %{^field_number => %{name_atom: field_name}} -> "field #{field_name}"
          _ -> "field_number #{field_number}"
        end

      msg =
        "insufficient data decoding #{field}, " <>
          "expected #{inspect(rest)} to be at least #{value} bytes"

      raise Protobuf.DecodeError, message: msg
    end
  end

  defp handle_value(<<rest::bits>>, field_number, wire_type, value, message, props) do
    case props.field_props do
      %{^field_number => %FieldProps{packed?: true, name_atom: name_atom} = prop} ->
        new_message = update_in_message(message, name_atom, &value_for_packed(value, &1, prop))
        build_message(rest, new_message, props)

      %{^field_number => %FieldProps{wire_type: ^wire_type} = prop} ->
        key = field_key(prop, props)
        new_message = update_in_message(message, key, &value_for_field(value, &1, prop))
        build_message(rest, new_message, props)

      # Repeated fields of primitive numeric types can be "packed". Their packed? flag will be
      # false, but they will be encoded as wire_delimited() one after the other. In proto2, this
      # is explicit ([packed=true] option). In proto3, it's by default. See:
      # https://developers.google.com/protocol-buffers/docs/encoding#packed
      %{^field_number => %FieldProps{repeated?: true, name_atom: name_atom} = prop}
      when wire_type == wire_delimited() ->
        new_message = update_in_message(message, name_atom, &value_for_packed(value, &1, prop))
        build_message(rest, new_message, props)

      %{^field_number => %FieldProps{wire_type: expected, name: field}} ->
        raise DecodeError,
          message: "wrong wire_type for field #{field}: got #{wire_type}, expected #{expected}"

      %{} ->
        %mod{__unknown_fields__: unknown_fields} = message

        new_message =
          case Protobuf.Extension.get_extension_props_by_tag(mod, field_number) do
            {ext_mod, %{field_props: %FieldProps{} = prop}} ->
              current_value = Protobuf.Extension.get(message, ext_mod, prop.name_atom, nil)
              new_value = value_for_field(value, current_value, prop)
              Protobuf.Extension.put(mod, message, ext_mod, prop.name_atom, new_value)

            # Unknown field (the list is reversed after decoding the whole message so that the
            # order of the unknown fields is kept)
            _ ->
              new_field = {field_number, wire_type, value}
              %{message | __unknown_fields__: [new_field | unknown_fields]}
          end

        build_message(rest, new_message, props)
    end
  end

  defp value_for_field(value, current, %FieldProps{embedded?: false} = prop) do
    %FieldProps{type: type, name_atom: name_atom, oneof: oneof, repeated?: repeated?} = prop

    val = Wire.decode(type, value)
    val = if oneof, do: {name_atom, val}, else: val

    if repeated? do
      # List.wrap/1 wraps nil into [].
      [val | List.wrap(current)]
    else
      val
    end
  end

  defp value_for_field(bin, current, %FieldProps{embedded?: true} = prop) do
    %FieldProps{type: type, map?: map?, oneof: oneof, name_atom: name_atom, repeated?: repeated?} =
      prop

    embed_msg = decode(bin, type)

    val =
      if map? do
        key = if is_nil(embed_msg.key), do: map_default(prop, :key), else: embed_msg.key
        value = if is_nil(embed_msg.value), do: map_default(prop, :value), else: embed_msg.value
        %{key => value}
      else
        embed_msg
      end

    val = if oneof, do: {name_atom, val}, else: val

    cond do
      repeated? ->
        # List.wrap/1 wraps nil into [].
        [val | List.wrap(current)]

      current && map? ->
        Map.merge(current, val)

      # If the field is embedded but not repeated, it means that we need to merge the existing
      # embedded message together with the new embedded message.
      current ->
        deep_merge(current, val, type.__message_props__())

      true ->
        val
    end
  end

  defp map_default(prop, key_or_value) do
    prop.type.__message_props__().field_props
    |> Enum.find(fn {_key, field_props} -> field_props.name_atom == key_or_value end)
    |> then(fn {_key, field_props} ->
      # Conformance only works when we use proto3 defaults here, even for proto2...
      Protobuf.DSL.field_default(:proto3, field_props)
    end)
  end

  defp deep_merge(_oneof1 = {tag1, val1}, oneof2 = {tag2, val2}, props) do
    if tag1 == tag2 do
      # If the field is a oneof, we merge its value and keep the tag.
      {tag1, deep_merge(val1, val2, props)}
    else
      # If the field is a oneof but not with the same tag, then the second one takes over
      # completely.
      oneof2
    end
  end

  # If the two fields to merge are the same message, we merge it by merging their fields.
  defp deep_merge(%mod{} = msg1, %mod{} = msg2, %MessageProps{syntax: syntax} = props) do
    merged_attributes =
      for {_number, field_prop} <- props.field_props do
        key = field_key(field_prop, props)
        value = deep_merge_field(Map.fetch!(msg1, key), Map.fetch!(msg2, key), field_prop, syntax)
        {key, value}
      end

    merged = struct!(mod, merged_attributes)

    # Merge extensions. Not 100% sure this is right but it doesn't break any tests nor any
    # conformance tests...
    case {Map.fetch(msg1, :__pb_extensions__), Map.fetch(msg2, :__pb_extensions__)} do
      {{:ok, %{} = ext1}, {:ok, %{} = ext2}} ->
        %{merged | __pb_extensions__: Map.merge(ext1, ext2)}

      {{:ok, ext1}, :error} ->
        %{merged | __pb_extensions__: ext1}

      _other ->
        merged
    end
  end

  # Merging lists means concatenating them.
  defp deep_merge_field(val1, val2, %FieldProps{repeated?: true}, _syntax) do
    val1 ++ val2
  end

  # Merge maps by, well, merging the maps.
  defp deep_merge_field(val1, val2, %FieldProps{map?: true}, _syntax) do
    Map.merge(val1, val2)
  end

  # Recursively go up and merge two embedded messages with their new "message props".
  defp deep_merge_field(val1, val2, %FieldProps{embedded?: true, type: type}, _syntax)
       when not is_nil(val1) and not is_nil(val2) do
    deep_merge(val1, val2, type.__message_props__())
  end

  # If the two fields are normal fields, then we pick the second one unless it's a default value.
  defp deep_merge_field(val1, val2, %FieldProps{} = prop, syntax) do
    default? = val2 == Protobuf.DSL.field_default(syntax, prop)
    if default?, do: val1, else: val2
  end

  # The "packed" flag is, essentially, a suggestion. If a field says it's packed, it could be
  # packed but it could also _not_ be. For this reason, here we're only decoding fields as packed
  # if we get a binary. Otherwise, we already decoded the field, so we pass this down to
  # value_for_field/3.
  # Reference in the docs:
  # https://developers.google.com/protocol-buffers/docs/encoding#packed
  # Reference comment from @britto:
  # https://github.com/elixir-protobuf/protobuf/pull/207#discussion_r758480828

  defp value_for_packed(bin, current, %FieldProps{type: type, wire_type: wire_type})
       when is_binary(bin) do
    # List.wrap/1 wraps nil into [].
    current = List.wrap(current)

    case wire_type do
      wire_varint() -> decode_varints(bin, type, current)
      wire_32bits() -> decode_fixed32(bin, type, current)
      wire_64bits() -> decode_fixed64(bin, type, current)
    end
  end

  defp value_for_packed(value, current, prop) do
    value_for_field(value, current, prop)
  end

  defp decode_varints(<<>>, _type, acc), do: acc

  defdecoderp decode_varints(type, acc) do
    decode_varints(rest, type, [Wire.decode(type, value) | acc])
  end

  defp decode_fixed32(<<n::bits-32, bin::bits>>, type, acc) do
    decode_fixed32(bin, type, [Wire.decode(type, n) | acc])
  end

  defp decode_fixed32(<<>>, _type, acc), do: acc

  defp decode_fixed64(<<n::bits-64, bin::bits>>, type, acc) do
    decode_fixed64(bin, type, [Wire.decode(type, n) | acc])
  end

  defp decode_fixed64(<<>>, _type, acc), do: acc

  defp reverse_repeated(message, repeated_fields) do
    Enum.reduce(repeated_fields, message, fn repeated_field, message_acc ->
      case message_acc do
        %{^repeated_field => values} when is_list(values) ->
          %{message_acc | repeated_field => Enum.reverse(values)}

        _other ->
          message_acc
      end
    end)
  end

  defp field_key(%FieldProps{oneof: nil, name_atom: key}, _message_props) do
    key
  end

  defp field_key(%FieldProps{oneof: oneof_number}, %MessageProps{oneof: oneofs}) do
    {key, _num} = Enum.find(oneofs, &match?({_key, ^oneof_number}, &1))
    key
  end

  defp update_in_message(message, key, update_fun) do
    current =
      case message do
        %_{^key => value} -> value
        %_{} -> nil
      end

    Map.put(message, key, update_fun.(current))
  end
end

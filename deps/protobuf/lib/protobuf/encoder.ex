defmodule Protobuf.Encoder do
  @moduledoc false

  import Bitwise, only: [bsl: 2, bor: 2]
  import Protobuf.Wire.Types

  alias Protobuf.{FieldProps, MessageProps, Wire, Wire.Varint}

  @spec encode_to_iodata(struct()) :: iodata()
  def encode_to_iodata(%mod{} = struct) do
    struct
    |> transform_module(mod)
    |> do_encode_to_iodata()
  end

  @spec encode(struct()) :: binary()
  def encode(%mod{} = struct) do
    struct
    |> transform_module(mod)
    |> encode_with_message_props(mod.__message_props__())
    |> IO.iodata_to_binary()
  end

  defp do_encode_to_iodata(%mod{} = struct) do
    encode_with_message_props(struct, mod.__message_props__())
  end

  defp encode_with_message_props(
         struct,
         %MessageProps{syntax: syntax, field_props: field_props, ordered_tags: ordered_tags} =
           props
       ) do
    oneofs = oneof_actual_vals(props, struct)

    # We encode the fields in order since some recommended conformance tests expect us to do so.
    encoded =
      for fnum <- ordered_tags,
          prop = Map.fetch!(field_props, fnum),
          encoded = encode_field(prop, struct, oneofs, syntax),
          encoded != :skip do
        encoded
      end

    encoded = [encoded | encode_unknown_fields(struct)]

    if syntax == :proto2 do
      [encoded | encode_extensions(struct)]
    else
      encoded
    end
  end

  defp encode_field(%FieldProps{name_atom: name, oneof: oneof} = prop, struct, oneofs, syntax) do
    val =
      if oneof do
        oneofs[name]
      else
        Map.get(struct, name, nil)
      end

    do_encode_field(class_field(prop), val, syntax, prop)
  rescue
    error ->
      struct_mod = struct.__struct__

      raise Protobuf.EncodeError,
        message:
          "Got error when encoding #{inspect(struct_mod)}##{prop.name_atom}: #{Exception.format(:error, error)}"
  end

  defp skip_field?(_syntax, [], _prop), do: true
  defp skip_field?(_syntax, val, _prop) when is_map(val), do: map_size(val) == 0
  defp skip_field?(:proto2, nil, %FieldProps{optional?: optional?}), do: optional?
  defp skip_field?(:proto2, value, %FieldProps{default: value, oneof: nil}), do: true

  defp skip_field?(:proto3, val, %FieldProps{proto3_optional?: true}),
    do: is_nil(val)

  defp skip_field?(:proto3, nil, _prop), do: true
  defp skip_field?(:proto3, 0, %FieldProps{oneof: nil}), do: true
  defp skip_field?(:proto3, +0.0, %FieldProps{oneof: nil}), do: true
  defp skip_field?(:proto3, "", %FieldProps{oneof: nil}), do: true
  defp skip_field?(:proto3, false, %FieldProps{oneof: nil}), do: true
  defp skip_field?(_syntax, _val, _prop), do: false

  defp do_encode_field(
         :normal,
         val,
         syntax,
         %FieldProps{encoded_fnum: fnum, type: type, repeated?: repeated?} = prop
       ) do
    if skip_field?(syntax, val, prop) or skip_enum?(syntax, val, prop) do
      :skip
    else
      apply_or_map(val, repeated?, &[fnum | Wire.encode(type, &1)])
    end
  end

  defp do_encode_field(:embedded, _val = nil, _syntax, _prop) do
    :skip
  end

  defp do_encode_field(
         :embedded,
         val,
         syntax,
         %FieldProps{encoded_fnum: fnum, repeated?: repeated?, map?: map?, type: type} = prop
       ) do
    apply_or_map(val, repeated? || map?, fn val ->
      val = transform_module(val, type)

      if skip_field?(syntax, val, prop) do
        ""
      else
        val = if map?, do: struct(type, %{key: elem(val, 0), value: elem(val, 1)}), else: val

        # so that oneof {:atom, val} can be encoded
        encoded = encode_from_type(type, val)
        byte_size = IO.iodata_length(encoded)
        [fnum | Varint.encode(byte_size)] ++ encoded
      end
    end)
  end

  defp do_encode_field(:packed, val, syntax, %FieldProps{type: type, encoded_fnum: fnum} = prop) do
    if skip_field?(syntax, val, prop) or skip_enum?(syntax, val, prop) do
      :skip
    else
      encoded = Enum.map(val, &Wire.encode(type, &1))
      byte_size = IO.iodata_length(encoded)
      [fnum | Varint.encode(byte_size)] ++ encoded
    end
  end

  defp encode_from_type(mod, msg) do
    case msg do
      %{__struct__: ^mod} ->
        do_encode_to_iodata(msg)

      %other_mod{} = struct ->
        raise Protobuf.EncodeError,
          message:
            "struct #{inspect(other_mod)} can't be encoded as #{inspect(mod)}: #{inspect(struct)}"

      _ ->
        do_encode_to_iodata(struct(mod, msg))
    end
  end

  defp encode_unknown_fields(%_{__unknown_fields__: unknown_fields} = _message) do
    Enum.map(unknown_fields, fn {fnum, wire_type, value} ->
      [encode_fnum(fnum, wire_type), Wire.encode_from_wire_type(wire_type, value)]
    end)
  end

  defp transform_module(message, module) do
    if transform_module = module.transform_module() do
      transform_module.encode(message, module)
    else
      message
    end
  end

  defp class_field(%FieldProps{wire_type: wire_delimited(), embedded?: true}), do: :embedded
  defp class_field(%FieldProps{repeated?: true, packed?: true}), do: :packed
  defp class_field(_prop), do: :normal

  @doc false
  @spec encode_fnum(integer, integer) :: binary
  def encode_fnum(fnum, wire_type) do
    fnum
    |> bsl(3)
    |> bor(wire_type)
    |> Varint.encode()
    |> IO.iodata_to_binary()
  end

  defp apply_or_map(val, _repeated? = true, func), do: Enum.map(val, func)
  defp apply_or_map(val, _repeated? = false, func), do: func.(val)

  defp skip_enum?(:proto2, _value, _prop), do: false
  defp skip_enum?(:proto3, _value, %FieldProps{proto3_optional?: true}), do: false
  defp skip_enum?(_syntax, _value, %FieldProps{enum?: false}), do: false

  defp skip_enum?(_syntax, _value, %FieldProps{enum?: true, oneof: oneof}) when not is_nil(oneof),
    do: false

  defp skip_enum?(_syntax, _value, %FieldProps{required?: true}), do: false
  defp skip_enum?(_syntax, value, %FieldProps{type: type}), do: enum_default?(type, value)

  defp enum_default?({:enum, enum_mod}, val) when is_atom(val), do: enum_mod.value(val) == 0
  defp enum_default?({:enum, _enum_mod}, val) when is_integer(val), do: val == 0
  defp enum_default?({:enum, _enum_mod}, list) when is_list(list), do: false

  # Returns a map of %{field_name => field_value} from oneofs. For example, if you have:
  # oneof body {
  #   string a = 1;
  #   string b = 2
  # }
  # Then this could return: %{a: "some value"}
  defp oneof_actual_vals(
         %MessageProps{field_tags: field_tags, field_props: field_props, oneof: oneof},
         struct
       ) do
    Enum.reduce(oneof, %{}, fn {field, index}, acc ->
      case Map.fetch(struct, field) do
        {:ok, {field_name, value}} when is_atom(field_name) ->
          oneof =
            case field_props[field_tags[field_name]] do
              %FieldProps{oneof: oneof} ->
                oneof

              nil ->
                raise Protobuf.EncodeError,
                  message:
                    "#{inspect(field_name)} wasn't found in #{inspect(struct.__struct__)}##{field}"
            end

          if oneof != index do
            raise Protobuf.EncodeError,
              message:
                "#{inspect(field_name)} doesn't belong to #{inspect(struct.__struct__)}##{field}"
          else
            Map.put(acc, field_name, value)
          end

        {:ok, nil} ->
          acc

        :error ->
          acc

        other ->
          raise Protobuf.EncodeError,
            message:
              "#{inspect(struct.__struct__)}##{field} should be {key, val}, got: #{inspect(other)}"
      end
    end)
  end

  defp encode_extensions(%mod{__pb_extensions__: pb_exts}) when is_map(pb_exts) do
    Enum.reduce(pb_exts, [], fn {{ext_mod, key}, val}, acc ->
      case Protobuf.Extension.get_extension_props(mod, ext_mod, key) do
        %{field_props: prop} ->
          case do_encode_field(class_field(prop), val, :proto2, prop) do
            :skip -> acc
            iodata -> [acc | iodata]
          end

        _ ->
          acc
      end
    end)
  end

  defp encode_extensions(_) do
    []
  end
end

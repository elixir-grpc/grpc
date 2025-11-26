defmodule Protobuf.JSON.Encode do
  @moduledoc false

  alias Protobuf.JSON.{EncodeError, Utils}

  @compile {:inline,
            encode_field: 3,
            encode_key: 2,
            maybe_repeat: 3,
            encode_float_types: 2,
            encode_int32_types: 2,
            encode_int64_types: 2,
            encode_bool: 1,
            encode_bytes: 1,
            encode_string: 1,
            encode_enum: 3,
            safe_enum_key: 2}

  @duration_seconds_range -315_576_000_000..315_576_000_000

  @built_in_google_messages [
    Google.Protobuf.FieldMask,
    Google.Protobuf.Duration,
    Google.Protobuf.Timestamp,
    Google.Protobuf.BytesValue,
    Google.Protobuf.Struct,
    Google.Protobuf.ListValue,
    Google.Protobuf.Value,
    Google.Protobuf.Empty,
    Google.Protobuf.Int32Value,
    Google.Protobuf.UInt32Value,
    Google.Protobuf.UInt64Value,
    Google.Protobuf.Int64Value,
    Google.Protobuf.FloatValue,
    Google.Protobuf.DoubleValue,
    Google.Protobuf.BoolValue,
    Google.Protobuf.StringValue,
    Google.Protobuf.Any
  ]

  @doc false
  @spec to_encodable(struct, keyword) :: map | {:error, EncodeError.t()}
  def to_encodable(%mod{} = struct, opts) do
    struct
    |> transform_module(mod)
    |> encodable(opts)
  end

  def encodable(%mod{} = struct, _opts) when mod == Google.Protobuf.Duration do
    case struct do
      %{seconds: seconds} when seconds not in @duration_seconds_range ->
        throw({:bad_duration, :seconds_outside_of_range, seconds})

      %{seconds: seconds, nanos: 0} ->
        Integer.to_string(seconds) <> "s"

      %{seconds: seconds, nanos: nanos} ->
        sign = if seconds < 0 or nanos < 0, do: "-", else: ""
        "#{sign}#{abs(seconds)}.#{Utils.format_nanoseconds(nanos)}s"
    end
  end

  def encodable(%mod{} = struct, _opts) when mod == Google.Protobuf.Timestamp do
    %{seconds: seconds, nanos: nanos} = struct

    case Protobuf.JSON.RFC3339.encode(seconds, nanos) do
      {:ok, string} -> string
      {:error, reason} -> throw({:invalid_timestamp, struct, reason})
    end
  end

  def encodable(%mod{}, _opts) when mod == Google.Protobuf.Empty do
    %{}
  end

  # We can't encode Google.Protobuf.Value as "NaN"/"Infinity"/"-Infinity" when
  # the value is a number, because then we wouldn't be able to go back to the right
  # number type. This is documented here:
  # https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.Value
  # Also see:
  # https://github.com/protocolbuffers/protobuf/commit/ca1cb1ba80ef18f5dccfb5b6ee7fa623ba6caab5
  nans = [:nan, :infinity, :negative_infinity]

  def encodable(%mod{kind: kind}, opts) when mod == Google.Protobuf.Value do
    case kind do
      {:string_value, string} -> string
      {:number_value, number} when is_number(number) -> number
      {:number_value, val} when val in unquote(nans) -> throw({:non_numeric_float, val})
      {:bool_value, bool} -> bool
      {:null_value, :NULL_VALUE} -> nil
      {:list_value, list} -> encodable(list, opts)
      {:struct_value, struct} -> encodable(struct, opts)
      _other -> throw({:bad_encoding, kind})
    end
  end

  def encodable(%mod{values: values}, opts) when mod == Google.Protobuf.ListValue do
    Enum.map(values, &encodable(&1, opts))
  end

  def encodable(%mod{fields: fields}, opts) when mod == Google.Protobuf.Struct do
    Map.new(fields, fn {key, val} -> {key, encodable(val, opts)} end)
  end

  def encodable(%mod{value: value}, _opts)
      when mod in [
             Google.Protobuf.Int32Value,
             Google.Protobuf.UInt32Value,
             Google.Protobuf.UInt64Value,
             Google.Protobuf.Int64Value,
             Google.Protobuf.FloatValue,
             Google.Protobuf.DoubleValue,
             Google.Protobuf.BoolValue,
             Google.Protobuf.StringValue
           ] do
    value
  end

  def encodable(%mod{value: value}, _opts) when mod == Google.Protobuf.BytesValue do
    Base.encode64(value)
  end

  def encodable(%mod{paths: paths}, _opts) when mod == Google.Protobuf.FieldMask do
    Enum.map_join(paths, ",", fn path ->
      cond do
        String.contains?(path, "__") ->
          throw({:bad_field_mask, paths})

        path =~ ~r/[A-Z0-9]/ ->
          throw({:bad_field_mask, path})

        true ->
          {first, rest} = path |> Macro.camelize() |> String.next_codepoint()
          String.downcase(first) <> rest
      end
    end)
  end

  def encodable(%mod{} = struct, opts) when mod == Google.Protobuf.Any do
    message_mod = Protobuf.Any.type_url_to_module(struct.type_url)

    value_to_encode =
      struct.value
      |> message_mod.decode()
      |> encodable(opts)

    if message_mod in @built_in_google_messages do
      %{"@type" => struct.type_url, "value" => value_to_encode}
    else
      Map.put(value_to_encode, "@type", struct.type_url)
    end
  end

  def encodable(%mod{} = struct, opts) do
    message_props = mod.__message_props__()
    regular = encode_regular_fields(struct, message_props, opts)
    oneofs = encode_oneof_fields(struct, message_props, opts)

    :maps.from_list(regular ++ oneofs)
  end

  defp encode_regular_fields(struct, %{field_props: field_props, syntax: syntax}, opts) do
    for {_field_num, %{name_atom: name, oneof: nil} = prop} <- field_props,
        %{^name => value} = struct,
        emit?(syntax, prop, value) || opts[:emit_unpopulated] do
      encode_field(prop, value, opts)
    end
  end

  defp encode_oneof_fields(struct, message_props, opts) do
    %{field_tags: field_tags, field_props: field_props, oneof: oneofs} = message_props

    for {oneof_name, _index} <- oneofs,
        tag_and_value = Map.get(struct, oneof_name) do
      {tag, value} = tag_and_value
      prop = field_props[field_tags[tag]]
      encode_field(prop, value, opts)
    end
  end

  # TODO: handle invalid values? check types?
  defp encode_field(prop, value, opts) do
    {encode_key(prop, opts), encode_value(value, prop, opts)}
  end

  defp encode_key(prop, opts) do
    if opts[:use_proto_names], do: prop.name, else: prop.json_name
  end

  @int32_types ~w(int32 sint32 sfixed32 fixed32 uint32)a
  @int64_types ~w(int64 sint64 sfixed64 fixed64 uint64)a
  @float_types [:float, :double]

  defp encode_value(nil, _prop, _opts), do: nil

  defp encode_value(value, %{type: :string} = prop, _opts) do
    maybe_repeat(prop, value, &encode_string/1)
  end

  defp encode_value(value, %{type: :bool} = prop, _opts) do
    maybe_repeat(prop, value, &encode_bool/1)
  end

  defp encode_value(value, %{type: type} = prop, _opts) when type in @int32_types do
    maybe_repeat(prop, value, &encode_int32_types(&1, type))
  end

  defp encode_value(value, %{type: type} = prop, _opts) when type in @int64_types do
    maybe_repeat(prop, value, &encode_int64_types(&1, type))
  end

  defp encode_value(value, %{type: :bytes} = prop, _opts) do
    maybe_repeat(prop, value, &encode_bytes/1)
  end

  defp encode_value(value, %{type: type} = prop, _opts) when type in @float_types do
    maybe_repeat(prop, value, &encode_float_types(&1, type))
  end

  defp encode_value(value, %{type: {:enum, enum}} = prop, opts) do
    maybe_repeat(prop, value, &encode_enum(enum, &1, opts))
  end

  # Map keys can be of any scalar type except float, double and bytes. Therefore, we need to
  # convert them to strings before encoding. Map values can be anything except another map.
  # According to the specs: "If you provide a key but no value for a map field, the behavior
  # when the field is serialized is language-dependent. In C++, Java, and Python the default
  # value for the type is serialized, while in other languages nothing is serialized". Here
  # we do serialize these values as `nil` by default.
  defp encode_value(map, %{map?: true, type: module}, opts) do
    %{field_props: field_props, field_tags: field_tags} = module.__message_props__()
    key_prop = field_props[field_tags[:key]]
    value_prop = field_props[field_tags[:value]]

    for {key, val} <- map, into: %{} do
      name = encode_value(key, key_prop, opts)
      value = encode_value(val, value_prop, opts)

      {to_string(name), value}
    end
  end

  defp encode_value(value, %{embedded?: true, type: type} = prop, opts) do
    maybe_repeat(prop, value, fn value ->
      value
      |> transform_module(type)
      |> encodable(opts)
    end)
  end

  defp encode_float_types(value, _type) when is_float(value), do: value
  defp encode_float_types(:negative_infinity, _type), do: "-Infinity"
  defp encode_float_types(:infinity, _type), do: "Infinity"
  defp encode_float_types(:nan, _type), do: "NaN"
  defp encode_float_types(value, type), do: throw({:invalid_type, type, value})

  # TODO: maybe define a helper for all enums messages, with strict validation.
  defp encode_enum(Google.Protobuf.NullValue, key, _opts) when key in [0, :NULL_VALUE] do
    nil
  end

  defp encode_enum(enum, key, opts) when is_atom(key) do
    case enum.mapping() do
      %{^key => tag} -> if opts[:use_enum_numbers], do: tag, else: key
      _other -> throw({:unknown_enum_value, key, enum})
    end
  end

  defp encode_enum(enum, num, opts) when is_integer(num) do
    if opts[:use_enum_numbers], do: num, else: safe_enum_key(enum, num)
  end

  defp encode_enum(_enum, key, _opts), do: throw({:invalid_type, :enum, key})

  defp encode_int32_types(value, _type) when is_integer(value), do: value
  defp encode_int32_types(value, type), do: throw({:invalid_type, type, value})

  defp encode_int64_types(value, _type) when is_integer(value), do: Integer.to_string(value)
  defp encode_int64_types(value, type), do: throw({:invalid_type, type, value})

  defp encode_bool(value) when is_boolean(value), do: value
  defp encode_bool(value), do: throw({:invalid_type, :bool, value})

  defp encode_bytes(value) when is_binary(value), do: Base.encode64(value)
  defp encode_bytes(value), do: throw({:invalid_type, :bytes, value})

  defp encode_string(value) when is_binary(value), do: value
  defp encode_string(value) when is_atom(value), do: value
  defp encode_string(value), do: throw({:invalid_type, :string, value})

  # proto3 allows unknown enum values, that is why we can't call enum.key(num) here.
  defp safe_enum_key(enum, num) do
    %{tags_map: tags_map, field_props: field_props} = enum.__message_props__()

    case field_props[tags_map[num]] do
      %{name_atom: key} -> key
      _ -> num
    end
  end

  defp maybe_repeat(%{repeated?: false}, val, fun), do: fun.(val)
  defp maybe_repeat(%{repeated?: true}, val, fun), do: Enum.map(val, fun)

  defp emit?(:proto2, %{default: value}, value), do: false
  defp emit?(:proto2, %{optional?: true}, val), do: not is_nil(val)
  defp emit?(:proto3, %{proto3_optional?: true}, val), do: not is_nil(val)
  defp emit?(_syntax, _prop, +0.0), do: false
  defp emit?(_syntax, _prop, nil), do: false
  defp emit?(_syntax, _prop, 0), do: false
  defp emit?(_syntax, _prop, false), do: false
  defp emit?(_syntax, _prop, []), do: false
  defp emit?(_syntax, _prop, ""), do: false
  defp emit?(_syntax, _prop, %{} = map) when map_size(map) == 0, do: false
  defp emit?(_syntax, %{type: {:enum, enum}}, key) when is_atom(key), do: enum.value(key) != 0
  defp emit?(_syntax, _prop, _value), do: true

  defp transform_module(message, module) do
    if transform_module = module.transform_module() do
      transform_module.encode(message, module)
    else
      message
    end
  end
end

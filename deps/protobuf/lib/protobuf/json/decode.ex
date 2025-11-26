defmodule Protobuf.JSON.Decode do
  @moduledoc false

  import Bitwise, only: [bsl: 2]

  alias Protobuf.JSON.Utils

  @compile {:inline,
            fetch_field_value: 2,
            decode_map: 2,
            decode_repeated: 2,
            decode_integer: 1,
            decode_float: 1,
            parse_float: 1,
            decode_bytes: 1,
            decode_key: 3,
            parse_key: 2}

  @int32_range -bsl(1, 31)..(bsl(1, 31) - 1)
  @int64_range -bsl(1, 63)..(bsl(1, 63) - 1)
  @uint32_range 0..(bsl(1, 32) - 1)
  @uint64_range 0..(bsl(1, 64) - 1)

  @int_ranges %{
    int32: @int32_range,
    int64: @int64_range,
    sint32: @int32_range,
    sint64: @int64_range,
    sfixed32: @int32_range,
    sfixed64: @int64_range,
    fixed32: @int32_range,
    fixed64: @int64_range,
    uint32: @uint32_range,
    uint64: @uint64_range
  }

  @int_types Map.keys(@int_ranges)

  max_float = 3.402823466e38
  @float_range {-max_float, max_float}

  @float_types [:float, :double]

  @duration_seconds_range -315_576_000_000..315_576_000_000

  @spec from_json_data(term(), module()) :: struct()
  def from_json_data(term, module)

  # We start with all the Google built-in types that have specially-defined JSON decoding rules.
  # These rules are listed here: https://developers.google.com/protocol-buffers/docs/proto3#json
  # Note that we always have to keep the module names for the built-in types dynamic because
  # these built-in types **do not ship with our library**.

  def from_json_data(string, Google.Protobuf.Duration = mod) when is_binary(string) do
    # We need to check the sign from the raw string itself and can't rely on Integer.parse/1. This
    # is because if seconds is 0, then we couldn't determine whether it was "0" or "-0". For
    # example, "-0.5s".
    sign = if String.starts_with?(string, "-"), do: -1, else: 1

    case Integer.parse(string) do
      {seconds, "s"} when seconds in @duration_seconds_range ->
        struct!(mod, seconds: seconds)

      {seconds, "." <> nanos_with_s} when seconds in @duration_seconds_range ->
        case Utils.parse_nanoseconds(nanos_with_s) do
          {nanos, "s"} -> struct!(mod, seconds: seconds, nanos: nanos * sign)
          :error -> throw({:bad_duration, string, nanos_with_s})
        end

      other ->
        throw({:bad_duration, string, other})
    end
  end

  def from_json_data(string, Google.Protobuf.Timestamp = mod) when is_binary(string) do
    case Protobuf.JSON.RFC3339.decode(string) do
      {:ok, seconds, nanos} -> struct!(mod, seconds: seconds, nanos: nanos)
      {:error, reason} -> throw({:bad_timestamp, string, reason})
    end
  end

  def from_json_data(map, Google.Protobuf.Empty = mod) when map == %{} do
    struct!(mod)
  end

  def from_json_data(int, Google.Protobuf.Int32Value = mod),
    do: struct!(mod, value: decode_scalar(:int32, :unknown_name, int))

  def from_json_data(int, Google.Protobuf.UInt32Value = mod),
    do: struct!(mod, value: decode_scalar(:uint32, :unknown_name, int))

  def from_json_data(int, Google.Protobuf.UInt64Value = mod),
    do: struct!(mod, value: decode_scalar(:uint64, :unknown_name, int))

  def from_json_data(int, Google.Protobuf.Int64Value = mod),
    do: struct!(mod, value: decode_scalar(:int64, :unknown_name, int))

  def from_json_data(number, mod)
      when mod in [
             Google.Protobuf.FloatValue,
             Google.Protobuf.DoubleValue
           ] and (is_float(number) or is_integer(number)) do
    struct!(mod, value: number * 1.0)
  end

  def from_json_data(bool, Google.Protobuf.BoolValue = mod) when is_boolean(bool) do
    struct!(mod, value: decode_scalar(:bool, :unknown_field, bool))
  end

  def from_json_data(string, Google.Protobuf.StringValue = mod) when is_binary(string) do
    struct!(mod, value: decode_scalar(:string, :unknown_field, string))
  end

  def from_json_data(bytes, Google.Protobuf.BytesValue = mod) when is_binary(bytes) do
    struct!(mod, value: decode_scalar(:bytes, :unknown_field, bytes))
  end

  def from_json_data(list, Google.Protobuf.ListValue = mod) when is_list(list) do
    struct!(mod, values: Enum.map(list, &from_json_data(&1, Google.Protobuf.Value)))
  end

  def from_json_data(struct, Google.Protobuf.Struct = mod) when is_map(struct) do
    fields =
      Map.new(struct, fn {key, val} -> {key, from_json_data(val, Google.Protobuf.Value)} end)

    struct!(mod, fields: fields)
  end

  def from_json_data(term, Google.Protobuf.Value = mod) do
    cond do
      is_nil(term) ->
        struct!(mod, kind: {:null_value, :NULL_VALUE})

      is_binary(term) ->
        struct!(mod, kind: {:string_value, term})

      is_integer(term) ->
        struct!(mod, kind: {:number_value, term * 1.0})

      is_float(term) ->
        struct!(mod, kind: {:number_value, term})

      is_boolean(term) ->
        struct!(mod, kind: {:bool_value, term})

      is_list(term) ->
        struct!(mod, kind: {:list_value, from_json_data(term, Google.Protobuf.ListValue)})

      is_map(term) ->
        struct!(mod, kind: {:struct_value, from_json_data(term, Google.Protobuf.Struct)})

      true ->
        throw({:bad_message, term, mod})
    end
  end

  def from_json_data(data, Google.Protobuf.FieldMask = mod) when is_binary(data) do
    paths = String.split(data, ",")

    cond do
      data == "" -> struct!(mod, paths: [])
      paths = Enum.map(paths, &convert_field_mask_to_underscore/1) -> struct!(mod, paths: paths)
      true -> throw({:bad_field_mask, data})
    end
  end

  def from_json_data(%{"@type" => type_url} = data, Google.Protobuf.Any = mod) do
    data = Map.delete(data, "@type")
    message_mod = Protobuf.Any.type_url_to_module(type_url)

    encoded =
      case Map.fetch(data, "value") do
        # Types with a built-in JSON representation (like google.protobuf.Timestamp) have a
        # "value" field with the JSON representation itself.
        # See: https://developers.google.com/protocol-buffers/docs/proto3#json
        {:ok, value} ->
          value
          |> from_json_data(message_mod)
          |> message_mod.encode()

        # When a message doesn't have a built-in JSON representation (like
        # google.protobuf.Timestamp), then it's encoded as a JSON object and then a @type field is
        # added with the type_url for that message.
        :error ->
          data
          |> from_json_data(message_mod)
          |> message_mod.encode()
      end

    struct!(mod, type_url: type_url, value: encoded)
  end

  def from_json_data(data, module) when is_map(data) and is_atom(module) do
    message_props = module.__message_props__()
    regular = decode_regular_fields(data, message_props)
    oneofs = decode_oneof_fields(data, message_props)

    module
    |> struct(regular)
    |> struct(oneofs)
    |> transform_module(module)
  end

  def from_json_data(data, module) when is_atom(module), do: throw({:bad_message, data, module})

  defp convert_field_mask_to_underscore(mask) do
    if mask =~ ~r/^[a-zA-Z0-9\.]+$/ do
      String.split(mask, ".")
      |> Enum.map(&Macro.underscore/1)
      |> Enum.join(".")
    else
      throw({:bad_field_mask, mask})
    end
  end

  defp decode_regular_fields(data, %{field_props: field_props}) do
    Enum.flat_map(field_props, fn
      {_field_num, %Protobuf.FieldProps{oneof: nil} = prop} ->
        case fetch_field_value(prop, data) do
          {:ok, value} ->
            case decode_value(prop, value) do
              nil -> []
              value -> [{prop.name_atom, value}]
            end

          :error ->
            []
        end

      {_field_num, _prop} ->
        []
    end)
  end

  defp decode_oneof_fields(data, %{field_props: field_props, oneof: oneofs}) do
    for {oneof, index} <- oneofs,
        {_field_num, %{oneof: ^index} = prop} <- field_props,
        result = fetch_field_value(prop, data),
        match?({:ok, _value}, result),
        {:ok, value} = result,
        not null_value?(value, prop) do
      {oneof, prop.name_atom, decode_value(prop, value)}
    end
    |> Enum.reduce(%{}, fn {oneof, name, decoded_value}, acc ->
      if Map.has_key?(acc, oneof) do
        throw({:duplicated_oneof, oneof})
      else
        Map.put(acc, oneof, {name, decoded_value})
      end
    end)
  end

  defp null_value?(nil, %Protobuf.FieldProps{type: {:enum, Google.Protobuf.NullValue}}), do: false
  defp null_value?(value, _props), do: is_nil(value)

  defp fetch_field_value(%Protobuf.FieldProps{name: name_key, json_name: json_key}, data) do
    case data do
      %{^json_key => value} -> {:ok, value}
      %{^name_key => value} -> {:ok, value}
      _ -> :error
    end
  end

  defp decode_value(%{optional?: true, type: type}, nil) when type != Google.Protobuf.Value,
    do: nil

  defp decode_value(%{map?: true} = prop, map), do: decode_map(prop, map)
  defp decode_value(%{repeated?: true} = prop, list), do: decode_repeated(prop, list)
  defp decode_value(%{repeated?: false} = prop, value), do: decode_singular(prop, value)

  defp decode_map(%{type: module, name_atom: field}, map) when is_map(map) do
    %{field_props: field_props, field_tags: field_tags} = module.__message_props__()
    key_type = field_props[field_tags[:key]].type
    val_prop = field_props[field_tags[:value]]

    for {key, val} <- map, into: %{} do
      {decode_key(key_type, key, field), decode_singular(val_prop, val)}
    end
  end

  defp decode_map(_prop, nil), do: nil

  defp decode_map(prop, bad_map), do: throw({:bad_map, prop.name_atom, bad_map})

  defp decode_key(type, key, field) when is_binary(key) do
    case parse_key(type, key) do
      {:ok, decoded} -> decoded
      :error -> throw({:bad_map_key, field, type, key})
    end
  end

  defp decode_key(type, key, field), do: throw({:bad_map_key, field, type, key})

  # Map keys can be of any scalar type except float, double and bytes. they
  # must always be wrapped in strings. Other types should not compile.
  defp parse_key(:string, key), do: {:ok, key}
  defp parse_key(:bool, "true"), do: {:ok, true}
  defp parse_key(:bool, "false"), do: {:ok, false}
  defp parse_key(type, key) when type in @int_types, do: parse_int(key)
  defp parse_key(_type, _key), do: :error

  defp decode_repeated(prop, value) when is_list(value) do
    for val <- value, do: decode_singular(prop, val)
  end

  defp decode_repeated(_prop, nil), do: nil

  defp decode_repeated(prop, value) do
    throw({:bad_repeated, prop.name_atom, value})
  end

  defp decode_singular(%{type: type} = prop, value)
       when type in [:string, :bool, :bytes] or type in @int_types or type in @float_types do
    decode_scalar(type, prop.name_atom, value)
  end

  defp decode_singular(%{type: {:enum, enum}} = prop, value) do
    Map.get_lazy(enum.__reverse_mapping__(), value, fn ->
      cond do
        is_integer(value) and value in @int32_range -> value
        is_nil(value) and enum == Google.Protobuf.NullValue -> :NULL_VALUE
        true -> throw({:bad_enum, prop.name_atom, value})
      end
    end)
  end

  defp decode_singular(%{type: module, embedded?: true}, value) do
    from_json_data(value, module)
  end

  defp decode_scalar(:string, name, value) do
    if is_binary(value), do: value, else: throw({:bad_string, name, value})
  end

  defp decode_scalar(:bool, name, value) do
    if is_boolean(value), do: value, else: throw({:bad_bool, name, value})
  end

  defp decode_scalar(type, name, value) when type in @int_types do
    with {:ok, integer} <- decode_integer(value),
         true <- integer in @int_ranges[type] do
      integer
    else
      _ -> throw({:bad_int, name, value})
    end
  end

  defp decode_scalar(type, name, value) when type in @float_types do
    {float_min, float_max} = @float_range

    # If the type is float, we check that it's in range. If the type is double, we don't need to
    # do that cause the BEAM would throw an error for an out of bounds double anyways.
    case decode_float(value) do
      {:ok, float}
      when type == :float and is_float(float) and (float < float_min or float > float_max) ->
        # Float is out of range.
        throw({:bad_float, name, value})

      {:ok, value} ->
        value

      :error ->
        throw({:bad_float, name, value})
    end
  end

  defp decode_scalar(:bytes, name, value) do
    with true <- is_binary(value),
         {:ok, bytes} <- decode_bytes(value) do
      bytes
    else
      _ -> throw({:bad_bytes, name})
    end
  end

  defp decode_integer(integer) when is_integer(integer), do: {:ok, integer}
  defp decode_integer(string) when is_binary(string), do: parse_int(string)
  defp decode_integer(float) when is_float(float), do: parse_float_as_int(float)
  defp decode_integer(_bad), do: :error

  defp parse_int(string) do
    case Integer.parse(string) do
      {int, ""} ->
        {:ok, int}

      _ ->
        # We accept integers in scientific notation as well:
        case Float.parse(string) do
          {float, ""} ->
            parse_float_as_int(float)

          _ ->
            :error
        end
    end
  end

  defp parse_float_as_int(float) do
    truncated = trunc(float)

    if float - truncated == 0.0 do
      {:ok, truncated}
    else
      :error
    end
  end

  defp decode_float(float) when is_float(float), do: {:ok, float}
  defp decode_float(integer) when is_integer(integer), do: {:ok, integer / 1}
  defp decode_float(string) when is_binary(string), do: parse_float(string)
  defp decode_float(_bad), do: :error

  defp parse_float("-Infinity"), do: {:ok, :negative_infinity}
  defp parse_float("Infinity"), do: {:ok, :infinity}
  defp parse_float("NaN"), do: {:ok, :nan}

  defp parse_float(string) do
    case Float.parse(string) do
      {float, ""} -> {:ok, float}
      _ -> :error
    end
  end

  # Both url-encoded and regular base64 are accepted, with and without padding.
  defp decode_bytes(bytes) do
    pattern = :binary.compile_pattern(["-", "_"])

    if String.contains?(bytes, pattern) do
      Base.url_decode64(bytes, padding: false)
    else
      Base.decode64(bytes, padding: false)
    end
  end

  defp transform_module(message, module) do
    if transform_module = module.transform_module() do
      transform_module.decode(message, module)
    else
      message
    end
  end
end

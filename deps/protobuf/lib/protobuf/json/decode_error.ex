defmodule Protobuf.JSON.DecodeError do
  defexception [:message]

  @type t :: %__MODULE__{message: String.t()}

  def new({:unsupported_syntax, syntax}) do
    %__MODULE__{message: "JSON encoding of '#{syntax}' syntax is unsupported, try proto3"}
  end

  def new(:no_json_lib) do
    %__MODULE__{message: "JSON library not loaded, make sure to add :jason to your mix.exs file"}
  end

  def new({:bad_message, data, module}) do
    %__MODULE__{message: "JSON map expected for module #{inspect(module)}, got: #{inspect(data)}"}
  end

  def new({:bad_duration, string, error}) do
    %__MODULE__{message: "bad JSON value for duration #{inspect(string)}, got: #{inspect(error)}"}
  end

  def new({:bad_timestamp, string, reason}) do
    %__MODULE__{
      message:
        "bad JSON value for timestamp #{inspect(string)}, failed to parse: #{inspect(reason)}"
    }
  end

  def new({:bad_field_mask, string}) do
    %__MODULE__{message: "invalid characters in field mask: #{inspect(string)}"}
  end

  def new({:bad_string, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid string (#{inspect(value)})"}
  end

  def new({:bad_bool, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid boolean (#{inspect(value)})"}
  end

  def new({:bad_int, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid integer (#{inspect(value)})"}
  end

  def new({:bad_float, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid floating point (#{inspect(value)})"}
  end

  def new({:bad_bytes, field}) do
    %__MODULE__{message: "Field '#{field}' has an invalid Base64-encoded byte sequence"}
  end

  def new({:bad_enum, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid enum value (#{inspect(value)})"}
  end

  def new({:bad_map, field, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid map (#{inspect(value)})"}
  end

  def new({:bad_map_key, field, type, value}) do
    %__MODULE__{message: "Field '#{field}' has an invalid map key (#{type}: #{inspect(value)})"}
  end

  def new({:duplicated_oneof, oneof}) do
    %__MODULE__{message: "Oneof field '#{oneof}' cannot be set twice"}
  end

  def new({:bad_repeated, field, value}) do
    %__MODULE__{message: "Repeated field '#{field}' expected a list, got #{inspect(value)}"}
  end

  def new({:unexpected_end, position}) do
    %__MODULE__{message: "Unexpected end at position #{inspect(position)}"}
  end

  def new({:invalid_byte, position, byte}) do
    %__MODULE__{message: "Invalid byte at position #{inspect(position)}, byte: #{inspect(byte)}"}
  end

  def new({:unexpected_sequence, position, sequence}) do
    %__MODULE__{
      message:
        "Unexpected sequence at position #{inspect(position)}, sequence: #{inspect(sequence)}"
    }
  end
end

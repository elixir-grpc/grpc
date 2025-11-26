defmodule Protobuf.JSON.EncodeError do
  defexception [:message]

  @type t :: %__MODULE__{message: String.t()}

  def new({:unsupported_syntax, syntax}) do
    %__MODULE__{message: "JSON encoding of '#{syntax}' syntax is unsupported, try proto3"}
  end

  def new(:no_json_lib) do
    %__MODULE__{message: "JSON library not loaded, make sure to add :jason to your mix.exs file"}
  end

  def new({:bad_duration, :seconds_outside_of_range, seconds}) do
    %__MODULE__{
      message: "invalid Google.Protobuf.Duration, seconds are outside of range: #{seconds}"
    }
  end

  def new({:invalid_timestamp, timestamp, reason}) do
    %__MODULE__{
      message:
        "invalid Google.Protobuf.Timestamp value #{inspect(timestamp)}, reason: #{inspect(reason)}"
    }
  end

  def new({:unknown_enum_value, key, enum_mod}) when is_atom(key) and is_atom(enum_mod) do
    %__MODULE__{
      message: "unknown value #{key} for enum #{inspect(enum_mod)}"
    }
  end

  def new({:invalid_type, type, value}) when is_atom(type) do
    %__MODULE__{
      message: "invalid value for type #{type}: #{inspect(value)}"
    }
  end

  def new({:non_numeric_float, nan}) when nan in [:nan, :infinity, :negative_infinity] do
    message = """
    cannot encode non-numeric float/double for Google.Protobuf.Value: #{inspect(nan)}
    See https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.Value
    """

    %__MODULE__{message: message}
  end

  def new({:bad_field_mask, mask}) do
    %__MODULE__{message: "unencodable field mask: #{inspect(mask)}"}
  end

  def new({:bad_encoding, term}) do
    %__MODULE__{message: "bad encoding: #{inspect(term)}"}
  end
end

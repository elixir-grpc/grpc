defmodule Protobuf.JSON do
  @moduledoc """
  JSON encoding and decoding utilities for Protobuf structs.

  It follows Google's [specs](https://developers.google.com/protocol-buffers/docs/proto3#json)
  and reference implementation. Some features
  such as [well-known](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf)
  types are not fully supported yet.

  Proto3 is supported as per the specification. Proto2 is supported in practice, but some of its
  features might not work correctly, such as extensions.

  ## Types

  | Protobuf                     | JSON                        | Supported |
  |------------------------------|-----------------------------|-----------|
  | `bool`                       | `true`/`false`              | Yes       |
  | `int32`, `fixed32`, `uint32` | Number                      | Yes       |
  | `int64`, `fixed64`, `uint64` | String                      | Yes       |
  | `float`, `double`            | Number                      | Yes       |
  | `bytes`                      | Base64 string               | Yes       |
  | `string`                     | String                      | Yes       |
  | `message`                    | Object (`{…}`)              | Yes       |
  | `enum`                       | String                      | Yes       |
  | `map<K,V>`                   | Object (`{…}`)              | Yes       |
  | `repeated V`                 | Array of `[v, …]`           | Yes       |
  | `Any`                        | Object (`{…}`)              | Yes       |
  | `Timestamp`                  | RFC3339 datetime            | Yes       |
  | `Duration`                   | String (`seconds.fraction`) | Yes       |
  | `Struct`                     | Object (`{…}`)              | Yes       |
  | `Wrapper types`              | Various types               | Yes       |
  | `FieldMask`                  | String                      | Yes       |
  | `ListValue`                  | Array                       | Yes       |
  | `Value`                      | Any JSON value              | Yes       |
  | `NullValue`                  | `null`                      | Yes       |
  | `Empty`                      | Object (`{…}`)              | Yes       |

  ## Usage

  `Protobuf.JSON` requires a JSON library to work, so first make sure you have `:jason` added
  to your dependencies:

      defp deps do
        [
          {:jason, "~> 1.2"},
          # ...
        ]
      end

  With `encode/1` you can turn any `Protobuf` message struct into a JSON string:

      iex> message = %Car{color: :RED, top_speed: 125.3}
      iex> Protobuf.JSON.encode(message)
      {:ok, "{\\"color\\":\\"RED\\",\\"topSpeed\\":125.3}"}

  And go the other way around with `decode/1`:

      iex> json = ~S|{"color":"RED","topSpeed":125.3}|
      iex> Protobuf.JSON.decode(json, Car)
      {:ok, %Car{color: :RED, top_speed: 125.3}}

  JSON keys are encoded as *camelCase strings* by default, specified by the `json_name` field
  option. So make sure to *recompile the `.proto` files in your project* before working with
  JSON encoding, the compiler will generate all the required `json_name` options. You can set
  your own `json_name` for a particular field too:

      message GeoCoordinate {
        double latitude = 1 [ json_name = "lat" ];
        double longitude = 2 [ json_name = "long" ];
      }

  ## Known Issues and Limitations

  Currently, the `protoc` compiler won't check for field name collisions. This library won't
  check that either. Make sure your field names will be unique when serialized to JSON.
  For instance, this message definition will not encode correctly since it will emit just
  one of the two fields and the problem might go unnoticed:

      message CollidingFields {
        int32 f1 = 1 [json_name = "sameName"];
        float f2 = 2 [json_name = "sameName"];
      }

  According to the specification, when duplicated JSON keys are found in maps, the library
  should raise a decoding error. It currently ignores duplicates and keeps the last occurrence.

  ## `google.protobuf.Any`

  The `google.protobuf.Any` type is supported. It can be used to encode and decode arbitrary
  messages. When decoding, the "type URL" is used to determine the message type to decode
  to. The type URL is expected to be in the format of
  `type.googleapis.com/<package>.<message>`. For example, the type URL for the
  `google.protobuf.Duration` message would be
  `type.googleapis.com/google.protobuf.Duration`. To determine the Elixir module from the
  type URL, the package and message names are split on `.` and transformed into a module
  name. In the previous example, we'd end up with `Google.Protobuf.Duration`. Due to
  arbitrary atom construction, we're forced to use `Module.safe_concat/1` to construct the
  module name. This means that the module must exist before decoding. If the module doesn't
  exist, decoding will raise an error.
  """

  alias Protobuf.JSON.{Encode, EncodeError, Decode, DecodeError, JSONLibrary}

  @type encode_opt() ::
          {:use_proto_names, boolean()}
          | {:use_enum_numbers, boolean()}
          | {:emit_unpopulated, boolean()}

  @type json_data() :: %{optional(binary) => any}

  @doc """
  Generates a JSON representation of the given protobuf `struct`.

  Similar to `encode/2` except it will unwrap the error tuple and raise in case of errors.

  ## Examples

      iex> Protobuf.JSON.encode!(%Car{top_speed: 80.0})
      ~S|{"topSpeed":80.0}|

  """
  @spec encode!(struct, [encode_opt]) :: String.t() | no_return
  def encode!(struct, opts \\ []) do
    case encode_to_iodata(struct, opts) do
      {:ok, iodata} -> IO.iodata_to_binary(iodata)
      {:error, error} -> raise error
    end
  end

  @doc """
  Generates a JSON representation of the given protobuf `struct`.

  ## Options

    * `:use_proto_names` - use original field `name` instead of the camelCase `json_name` for
      JSON keys. Defaults to `false`.
    * `:use_enum_numbers` - encode `enum` field values as numbers instead of their labels.
      Defaults to `false`.
    * `:emit_unpopulated` - emit all fields, even when they are blank, empty, or set to their
      default value. Defaults to `false`.

  ## Examples

  Suppose that this is you Protobuf message:

      syntax = "proto3";

      message Car {
        enum Color {
          GREEN = 0;
          RED = 1;
        }

        Color color = 1;
        float top_speed = 2;
      }

  Encoding is as simple as:

      iex> Protobuf.JSON.encode(%Car{color: :RED, top_speed: 125.3})
      {:ok, ~S|{"color":"RED","topSpeed":125.3}|}

      iex> Protobuf.JSON.encode(%Car{color: :GREEN})
      {:ok, "{}"}

      iex> Protobuf.JSON.encode(%Car{}, emit_unpopulated: true)
      {:ok, ~S|{"color":"GREEN","topSpeed":0.0}|}

  """
  @spec encode(struct, [encode_opt]) ::
          {:ok, String.t()} | {:error, EncodeError.t() | Exception.t()}
  def encode(struct, opts \\ []) when is_list(opts) do
    case encode_to_iodata(struct, opts) do
      {:ok, iodata} -> {:ok, IO.iodata_to_binary(iodata)}
      {:error, error} -> {:error, error}
    end
  end

  @doc """
  Similar to `encode!/2`, but returns iodata

  See `encode_to_iodata/2` for more information about when this function should
  be preferred over `encode!/2`.
  """
  @spec encode_to_iodata!(struct, [encode_opt]) :: iodata()
  def encode_to_iodata!(struct, opts \\ []) when is_list(opts) do
    case encode_to_iodata(struct, opts) do
      {:ok, iodata} -> iodata
      {:error, error} -> raise error
    end
  end

  @doc """
  Similar to `encode/2`, but returns iodata

  This function should be preferred to encode/2, if the generated JSON will be
  handed over to one of the IO functions or sent over the socket. The Erlang
  runtime is able to leverage vectorised writes and avoid allocating a continuous
  buffer for the whole resulting string, lowering memory use and increasing
  performance.
  """
  @spec encode_to_iodata(struct, [encode_opt]) ::
          {:ok, iodata()} | {:error, EncodeError.t() | Exception.t()}
  def encode_to_iodata(%_{} = struct, opts \\ []) when is_list(opts) do
    with {:ok, map} <- to_encodable(struct, opts), do: JSONLibrary.encode_to_iodata(map)
  end

  @doc """
  Generates a JSON-encodable map for the given Protobuf `struct`.

  Similar to `encode/2` except it will return an intermediate `map` representation.
  This is especially useful if you want to use custom JSON encoding or a custom
  JSON library.

  Supports the same options as `encode/2`.

  ## Examples

      iex> Protobuf.JSON.to_encodable(%Car{color: :RED, top_speed: 125.3})
      {:ok, %{"color" => :RED, "topSpeed" => 125.3}}

      iex> Protobuf.JSON.to_encodable(%Car{color: :GREEN})
      {:ok, %{}}

      iex> Protobuf.JSON.to_encodable(%Car{}, emit_unpopulated: true)
      {:ok, %{"color" => :GREEN, "topSpeed" => 0.0}}

  """
  boolean_opts = [:use_proto_names, :use_enum_numbers, :emit_unpopulated]

  @spec to_encodable(struct, [encode_opt]) :: {:ok, json_data} | {:error, EncodeError.t()}
  def to_encodable(%_{} = struct, opts \\ []) when is_list(opts) do
    Enum.each(opts, fn
      {key, value} when key in unquote(boolean_opts) and is_boolean(value) ->
        :ok

      {key, value} when key in unquote(boolean_opts) ->
        raise ArgumentError, "option #{inspect(key)} must be a boolean, got: #{inspect(value)}"

      {key, _value} ->
        raise ArgumentError, "unknown option: #{inspect(key)}"

      other ->
        raise ArgumentError, "invalid element in options list: #{inspect(other)}"
    end)

    {:ok, Encode.to_encodable(struct, opts)}
  catch
    error -> {:error, EncodeError.new(error)}
  end

  @doc """
  Decodes a JSON `iodata` into a `module` Protobuf struct.

  Similar to `decode!/2` except it will unwrap the error tuple and raise in case of errors.

  ## Examples

      iex> Protobuf.JSON.decode!("{}", Car)
      %Car{color: :GREEN, top_speed: 0.0}

      iex> ~S|{"color":"RED"}| |> Protobuf.JSON.decode!(Car)
      %Car{color: :RED, top_speed: 0.0}

      iex> ~S|{"color":"GREEN","topSpeed":80.0}| |> Protobuf.JSON.decode!(Car)
      %Car{color: :GREEN, top_speed: 80.0}

  """
  @spec decode!(iodata, module) :: struct | no_return
  def decode!(iodata, module) do
    case decode(iodata, module) do
      {:ok, json} -> json
      {:error, error} -> raise error
    end
  end

  @doc """
  Decodes a JSON `iodata` into a `module` Protobuf struct.

  ## Examples

  Given this Protobuf message:

      syntax = "proto3";

      message Car {
        enum Color {
          GREEN = 0;
          RED = 1;
        }

        Color color = 1;
        float top_speed = 2;
      }

  You can build its structs from JSON like this:

      iex> Protobuf.JSON.decode("{}", Car)
      {:ok, %Car{color: :GREEN, top_speed: 0.0}}

      iex> ~S|{"color":"RED"}| |> Protobuf.JSON.decode(Car)
      {:ok, %Car{color: :RED, top_speed: 0.0}}

      iex> ~S|{"color":"GREEN","topSpeed":80.0}| |> Protobuf.JSON.decode(Car)
      {:ok, %Car{color: :GREEN, top_speed: 80.0}}

  """
  @spec decode(iodata, module) :: {:ok, struct} | {:error, DecodeError.t() | Exception.t()}
  def decode(iodata, module) when is_atom(module) do
    case JSONLibrary.decode(iodata) do
      {:ok, json_data} ->
        from_decoded(json_data, module)

      {:error, exception} ->
        {:error, exception}
    end
  end

  @doc """
  Decodes a `json_data` map into a `module` Protobuf struct.

  Similar to `decode/2` except it takes a JSON `map` representation of the data.
    This is especially useful if you want to use custom JSON encoding or a custom
  JSON library.

  ## Examples

      iex> Protobuf.JSON.from_decoded(%{}, Car)
      {:ok, %Car{color: :GREEN, top_speed: 0.0}}

      iex> Protobuf.JSON.from_decoded(%{"color" => "RED"}, Car)
      {:ok, %Car{color: :RED, top_speed: 0.0}}

      iex> Protobuf.JSON.from_decoded(%{"color" => "GREEN","topSpeed" => 80.0}, Car)
      {:ok, %Car{color: :GREEN, top_speed: 80.0}}

  """
  @spec from_decoded(json_data(), module()) :: {:ok, struct()} | {:error, DecodeError.t()}
  def from_decoded(json_data, module) when is_atom(module) do
    {:ok, Decode.from_json_data(json_data, module)}
  catch
    error -> {:error, DecodeError.new(error)}
  end
end

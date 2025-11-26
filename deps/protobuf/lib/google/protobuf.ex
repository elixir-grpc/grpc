defmodule Google.Protobuf do
  @moduledoc """
  Utility functions for working with Google Protobuf structs.
  """

  @doc """
  Converts a `Google.Protobuf.Struct` struct to a `t:map()` recursively
  converting values to their Elixir equivalents.

  ## Examples

      iex> to_map(%Google.Protobuf.Struct{})
      %{}

      iex> to_map(%Google.Protobuf.Struct{
      ...>   fields: %{
      ...>     "key_one" => %Google.Protobuf.Value{
      ...>       kind: {:string_value, "value_one"},
      ...>     },
      ...>     "key_two" => %Google.Protobuf.Value{
      ...>       kind: {:number_value, 1234.0},
      ...>     }
      ...>   },
      ...> })
      %{"key_one" => "value_one", "key_two" => 1234.0}

  """
  @spec to_map(Google.Protobuf.Struct.t()) :: map()
  def to_map(struct) do
    Map.new(struct.fields, fn {k, v} ->
      {k, to_map_value(v)}
    end)
  end

  defp to_map_value(%{kind: {:null_value, :NULL_VALUE}}), do: nil
  defp to_map_value(%{kind: {:number_value, value}}), do: value
  defp to_map_value(%{kind: {:string_value, value}}), do: value
  defp to_map_value(%{kind: {:bool_value, value}}), do: value

  defp to_map_value(%{kind: {:struct_value, struct}}),
    do: to_map(struct)

  defp to_map_value(%{kind: {:list_value, %{values: values}}}),
    do: Enum.map(values, &to_map_value/1)

  @doc """
  Converts a `t:map()` to a `Google.Protobuf.Struct` struct recursively
  wrapping values in their `Google.Protobuf.Value` equivalents.

  ## Examples

      iex> from_map(%{})
      %Google.Protobuf.Struct{}

  """
  @spec from_map(map()) :: Google.Protobuf.Struct.t()
  def from_map(map) do
    struct(Google.Protobuf.Struct, %{
      fields:
        Map.new(map, fn {k, v} ->
          {to_string(k), from_map_value(v)}
        end)
    })
  end

  defp from_map_value(nil) do
    struct(Google.Protobuf.Value, %{kind: {:null_value, :NULL_VALUE}})
  end

  defp from_map_value(value) when is_number(value) do
    struct(Google.Protobuf.Value, %{kind: {:number_value, value}})
  end

  defp from_map_value(value) when is_binary(value) do
    struct(Google.Protobuf.Value, %{kind: {:string_value, value}})
  end

  defp from_map_value(value) when is_boolean(value) do
    struct(Google.Protobuf.Value, %{kind: {:bool_value, value}})
  end

  defp from_map_value(value) when is_map(value) do
    struct(Google.Protobuf.Value, %{kind: {:struct_value, from_map(value)}})
  end

  defp from_map_value(value) when is_list(value) do
    struct(Google.Protobuf.Value, %{
      kind:
        {:list_value,
         struct(Google.Protobuf.ListValue, %{
           values: Enum.map(value, &from_map_value/1)
         })}
    })
  end

  @doc """
  Converts a `DateTime` struct to a `Google.Protobuf.Timestamp` struct.

  Note: Elixir `DateTime.from_unix!/2` will convert units to
  microseconds internally. Nanosecond precision is not guaranteed.
  See examples for details.

  ## Examples

      iex> to_datetime(%Google.Protobuf.Timestamp{seconds: 5, nanos: 0})
      ~U[1970-01-01 00:00:05.000000Z]

      iex> one = to_datetime(%Google.Protobuf.Timestamp{seconds: 10, nanos: 100})
      ...> two = to_datetime(%Google.Protobuf.Timestamp{seconds: 10, nanos: 105})
      ...> DateTime.diff(one, two, :nanosecond)
      0

  """
  @spec to_datetime(Google.Protobuf.Timestamp.t()) :: DateTime.t()
  def to_datetime(%{seconds: seconds, nanos: nanos}) do
    DateTime.from_unix!(seconds * 1_000_000_000 + nanos, :nanosecond)
  end

  @doc """
    Converts a `Google.Protobuf.Timestamp` struct to a `DateTime` struct.

    ## Examples

        iex> from_datetime(~U[1970-01-01 00:00:05.000000Z])
        %Google.Protobuf.Timestamp{seconds: 5, nanos: 0}

  """
  @spec from_datetime(DateTime.t()) :: Google.Protobuf.Timestamp.t()
  def from_datetime(%DateTime{} = datetime) do
    nanoseconds = DateTime.to_unix(datetime, :nanosecond)

    struct(Google.Protobuf.Timestamp, %{
      seconds: div(nanoseconds, 1_000_000_000),
      nanos: rem(nanoseconds, 1_000_000_000)
    })
  end

  if Code.ensure_loaded?(Duration) do
    @doc """
    Converts a `Google.Protobuf.Duration` struct to a `Duration` struct.

    > #### Requires `Duration` {: .warning}
    > This function requires `Duration`, which was introduced in Elixir 1.17.

    ## Examples

        iex> to_duration(%Google.Protobuf.Duration{seconds: 1, nanos: 500_000_000})
        Duration.new!(second: 1, microsecond: {500_000, 6})
    """
    @doc since: "0.15.0"
    @spec to_duration(Google.Protobuf.Duration.t()) :: Duration.t()
    def to_duration(%Google.Protobuf.Duration{} = duration) do
      Duration.new!(second: duration.seconds, microsecond: {div(duration.nanos, 1_000), 6})
    end

    @doc """
    Converts a `Duration` struct to a `Google.Protobuf.Duration` struct.

    > #### Requires `Duration` {: .warning}
    > This function requires `Duration`, which was introduced in Elixir 1.17.

    ## Examples

        iex> from_duration(Duration.new!(second: 1, microsecond: {500_000, 6}))
        %Google.Protobuf.Duration{seconds: 1, nanos: 500_000_000}

        iex> from_duration(Duration.new!(hour: 1, minute: 2, microsecond: {500_000, 6}))
        %Google.Protobuf.Duration{seconds: 3720, nanos: 500_000_000}

        iex> from_duration(Duration.new!(minute: 2, microsecond: {6_500_000, 6}))
        %Google.Protobuf.Duration{seconds: 126, nanos: 500_000_000}
    """
    @doc since: "0.15.0"
    @spec from_duration(Duration.t()) :: Google.Protobuf.Duration.t()
    def from_duration(%Duration{} = duration) do
      {microsecond, _precision} = duration.microsecond
      seconds = div(to_timeout(duration), 1000)

      struct(Google.Protobuf.Duration, %{
        seconds: seconds,
        nanos: rem(microsecond, 1_000_000) * 1_000
      })
    end
  end
end

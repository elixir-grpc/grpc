defmodule GRPC.TimeUtils do
  @moduledoc false

  @doc """
  Returns relative time in ms.

  ## Examples

      iex> from = DateTime.utc_now
      iex> us = DateTime.to_unix(from, :microseconds)
      iex> datetime = DateTime.from_unix!(us + 5, :microseconds)
      iex> GRPC.TimeUtils.to_relative(datetime, from)
      5
  """
  def to_relative(datetime, from \\ DateTime.utc_now) do
    ms = datetime_to_microsecond(datetime)
    now_ms = datetime_to_microsecond(from)
    ms - now_ms
  end

  defp datetime_to_microsecond(datetime) do
    DateTime.to_unix(datetime) * 1000_000 + elem(datetime.microsecond, 0)
  end
end

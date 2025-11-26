defmodule GRPC.TimeUtils do
  @moduledoc false

  @doc """
  Returns relative time in milliseconds.

  ## Examples

      iex> from = DateTime.utc_now
      iex> us = DateTime.to_unix(from, :microsecond)
      iex> datetime = DateTime.from_unix!(us + 5005, :microsecond)
      iex> Float.round(GRPC.TimeUtils.to_relative(datetime, from), 3)
      5.005
  """
  def to_relative(datetime, from \\ DateTime.utc_now()) do
    ms = datetime_to_milliseconds(datetime)
    now_ms = datetime_to_milliseconds(from)
    ms - now_ms
  end

  defp datetime_to_milliseconds(datetime) do
    DateTime.to_unix(datetime, :second) * 1000 + elem(datetime.microsecond, 0) * 0.001
  end
end

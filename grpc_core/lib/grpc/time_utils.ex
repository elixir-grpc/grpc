defmodule GRPC.TimeUtils do
  @moduledoc false

  @doc """
  Returns relative time in whole milliseconds, truncated so a deadline is never extended.

  ## Examples

      iex> from = DateTime.utc_now
      iex> us = DateTime.to_unix(from, :microsecond)
      iex> datetime = DateTime.from_unix!(us + 5005, :microsecond)
      iex> GRPC.TimeUtils.to_relative(datetime, from)
      5
  """
  def to_relative(datetime, from \\ DateTime.utc_now()) do
    datetime |> DateTime.diff(from, :microsecond) |> div(1000)
  end
end

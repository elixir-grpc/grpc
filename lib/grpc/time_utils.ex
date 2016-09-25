defmodule GRPC.TimeUtils do
  def to_relative(datetime, from \\ DateTime.utc_now) do
    ms = datetime_to_microsecond(datetime)
    now_ms = datetime_to_microsecond(from)
    ms - now_ms
  end

  defp datetime_to_microsecond(datetime) do
    DateTime.to_unix(datetime) * 1000_000 + elem(datetime.microsecond, 0)
  end
end

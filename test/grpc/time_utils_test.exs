defmodule GRPC.TimeUtilsTest do
  use ExUnit.Case, async: true

  alias GRPC.TimeUtils

  test "to_relative/2 returns relative duration" do
    from = DateTime.utc_now
    us = DateTime.to_unix(from, :microseconds)
    datetime = DateTime.from_unix!(us + 5, :microseconds)
    assert TimeUtils.to_relative(datetime, from) == 5
  end
end

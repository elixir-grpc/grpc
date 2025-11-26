defmodule Google.Type.Interval do
  @moduledoc """
  Represents a time interval, encoded as a Timestamp start (inclusive) and a
  Timestamp end (exclusive).

  The start must be less than or equal to the end.
  When the start equals the end, the interval is empty (matches no time).
  When both start and end are unspecified, the interval matches any time.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :start_time, 1, type: Google.Protobuf.Timestamp, json_name: "startTime"
  field :end_time, 2, type: Google.Protobuf.Timestamp, json_name: "endTime"
end

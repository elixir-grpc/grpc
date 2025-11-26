defmodule Google.Type.DateTime do
  @moduledoc """
  Represents civil time (or occasionally physical time).

  This type can represent a civil time in one of a few possible ways:

   * When utc_offset is set and time_zone is unset: a civil time on a calendar
     day with a particular offset from UTC.
   * When time_zone is set and utc_offset is unset: a civil time on a calendar
     day in a particular time zone.
   * When neither time_zone nor utc_offset is set: a civil time on a calendar
     day in local time.

  The date is relative to the Proleptic Gregorian Calendar.

  If year is 0, the DateTime is considered not to have a specific year. month
  and day must have valid, non-zero values.

  This type may also be used to represent a physical time if all the date and
  time fields are set and either case of the `time_offset` oneof is set.
  Consider using `Timestamp` message for physical time instead. If your use
  case also would like to store the user's timezone, that can be done in
  another field.

  This type is more flexible than some applications may want. Make sure to
  document and validate your application's limitations.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :time_offset, 0

  field :year, 1, type: :int32
  field :month, 2, type: :int32
  field :day, 3, type: :int32
  field :hours, 4, type: :int32
  field :minutes, 5, type: :int32
  field :seconds, 6, type: :int32
  field :nanos, 7, type: :int32
  field :utc_offset, 8, type: Google.Protobuf.Duration, json_name: "utcOffset", oneof: 0
  field :time_zone, 9, type: Google.Type.TimeZone, json_name: "timeZone", oneof: 0
end

defmodule Google.Type.TimeZone do
  @moduledoc """
  Represents a time zone from the
  [IANA Time Zone Database](https://www.iana.org/time-zones).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :id, 1, type: :string
  field :version, 2, type: :string
end

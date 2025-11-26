defmodule Google.Type.Date do
  @moduledoc """
  Represents a whole or partial calendar date, such as a birthday. The time of
  day and time zone are either specified elsewhere or are insignificant. The
  date is relative to the Gregorian Calendar. This can represent one of the
  following:

  * A full date, with non-zero year, month, and day values
  * A month and day value, with a zero year, such as an anniversary
  * A year on its own, with zero month and day values
  * A year and month value, with a zero day, such as a credit card expiration
  date

  Related types are [google.type.TimeOfDay][google.type.TimeOfDay] and
  `google.protobuf.Timestamp`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :year, 1, type: :int32
  field :month, 2, type: :int32
  field :day, 3, type: :int32
end

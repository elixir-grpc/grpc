defmodule Google.Type.CalendarPeriod do
  @moduledoc """
  A `CalendarPeriod` represents the abstract concept of a time period that has
  a canonical start. Grammatically, "the start of the current
  `CalendarPeriod`." All calendar times begin at midnight UTC.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :CALENDAR_PERIOD_UNSPECIFIED, 0
  field :DAY, 1
  field :WEEK, 2
  field :FORTNIGHT, 3
  field :MONTH, 4
  field :QUARTER, 5
  field :HALF, 6
  field :YEAR, 7
end

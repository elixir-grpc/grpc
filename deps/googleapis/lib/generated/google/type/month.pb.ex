defmodule Google.Type.Month do
  @moduledoc """
  Represents a month in the Gregorian calendar.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :MONTH_UNSPECIFIED, 0
  field :JANUARY, 1
  field :FEBRUARY, 2
  field :MARCH, 3
  field :APRIL, 4
  field :MAY, 5
  field :JUNE, 6
  field :JULY, 7
  field :AUGUST, 8
  field :SEPTEMBER, 9
  field :OCTOBER, 10
  field :NOVEMBER, 11
  field :DECEMBER, 12
end

defmodule Google.Type.DayOfWeek do
  @moduledoc """
  Represents a day of the week.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :DAY_OF_WEEK_UNSPECIFIED, 0
  field :MONDAY, 1
  field :TUESDAY, 2
  field :WEDNESDAY, 3
  field :THURSDAY, 4
  field :FRIDAY, 5
  field :SATURDAY, 6
  field :SUNDAY, 7
end

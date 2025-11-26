defmodule Google.Type.TimeOfDay do
  @moduledoc """
  Represents a time of day. The date and time zone are either not significant
  or are specified elsewhere. An API may choose to allow leap seconds. Related
  types are [google.type.Date][google.type.Date] and
  `google.protobuf.Timestamp`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :hours, 1, type: :int32
  field :minutes, 2, type: :int32
  field :seconds, 3, type: :int32
  field :nanos, 4, type: :int32
end

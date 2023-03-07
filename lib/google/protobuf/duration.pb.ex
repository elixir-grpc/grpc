defmodule Google.Protobuf.Duration do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :seconds, 1, type: :int64
  field :nanos, 2, type: :int32
end

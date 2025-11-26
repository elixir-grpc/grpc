defmodule Google.Type.Fraction do
  @moduledoc """
  Represents a fraction in terms of a numerator divided by a denominator.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :numerator, 1, type: :int64
  field :denominator, 2, type: :int64
end

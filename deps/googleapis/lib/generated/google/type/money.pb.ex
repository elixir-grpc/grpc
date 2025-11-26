defmodule Google.Type.Money do
  @moduledoc """
  Represents an amount of money with its currency type.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :currency_code, 1, type: :string, json_name: "currencyCode"
  field :units, 2, type: :int64
  field :nanos, 3, type: :int32
end

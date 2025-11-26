defmodule Google.Api.FieldBehavior do
  @moduledoc """
  An indicator of the behavior of a given field (for example, that a field
  is required in requests, or given as output but ignored as input).
  This **does not** change the behavior in protocol buffers itself; it only
  denotes the behavior and may affect how API tooling handles the field.

  Note: This enum **may** receive new values in the future.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :FIELD_BEHAVIOR_UNSPECIFIED, 0
  field :OPTIONAL, 1
  field :REQUIRED, 2
  field :OUTPUT_ONLY, 3
  field :INPUT_ONLY, 4
  field :IMMUTABLE, 5
  field :UNORDERED_LIST, 6
  field :NON_EMPTY_DEFAULT, 7
  field :IDENTIFIER, 8
end

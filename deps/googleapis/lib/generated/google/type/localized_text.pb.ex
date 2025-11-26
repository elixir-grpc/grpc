defmodule Google.Type.LocalizedText do
  @moduledoc """
  Localized variant of a text in a particular language.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :text, 1, type: :string
  field :language_code, 2, type: :string, json_name: "languageCode"
end

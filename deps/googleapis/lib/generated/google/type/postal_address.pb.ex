defmodule Google.Type.PostalAddress do
  @moduledoc """
  Represents a postal address, e.g. for postal delivery or payments addresses.
  Given a postal address, a postal service can deliver items to a premise, P.O.
  Box or similar.
  It is not intended to model geographical locations (roads, towns,
  mountains).

  In typical usage an address would be created via user input or from importing
  existing data, depending on the type of process.

  Advice on address input / editing:
   - Use an i18n-ready address widget such as
     https://github.com/google/libaddressinput)
  - Users should not be presented with UI elements for input or editing of
    fields outside countries where that field is used.

  For more guidance on how to use this schema, please see:
  https://support.google.com/business/answer/6397478
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :revision, 1, type: :int32
  field :region_code, 2, type: :string, json_name: "regionCode"
  field :language_code, 3, type: :string, json_name: "languageCode"
  field :postal_code, 4, type: :string, json_name: "postalCode"
  field :sorting_code, 5, type: :string, json_name: "sortingCode"
  field :administrative_area, 6, type: :string, json_name: "administrativeArea"
  field :locality, 7, type: :string
  field :sublocality, 8, type: :string
  field :address_lines, 9, repeated: true, type: :string, json_name: "addressLines"
  field :recipients, 10, repeated: true, type: :string
  field :organization, 11, type: :string
end

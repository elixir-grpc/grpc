defmodule Google.Type.LatLng do
  @moduledoc """
  An object that represents a latitude/longitude pair. This is expressed as a
  pair of doubles to represent degrees latitude and degrees longitude. Unless
  specified otherwise, this must conform to the
  <a href="http://www.unoosa.org/pdf/icg/2012/template/WGS_84.pdf">WGS84
  standard</a>. Values must be within normalized ranges.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :latitude, 1, type: :double
  field :longitude, 2, type: :double
end

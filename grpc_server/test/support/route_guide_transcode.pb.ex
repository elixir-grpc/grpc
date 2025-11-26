defmodule RouteguideTranscode.Point do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :latitude, 1, type: :int32
  field :longitude, 2, type: :int32
end

defmodule RouteguideTranscode.Rectangle do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :lo, 1, type: RouteguideTranscode.Point
  field :hi, 2, type: RouteguideTranscode.Point
end

defmodule RouteguideTranscode.Feature do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :location, 2, type: RouteguideTranscode.Point
end

defmodule RouteguideTranscode.RouteNote do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :location, 1, type: RouteguideTranscode.Point
  field :message, 2, type: :string
end

defmodule RouteguideTranscode.RouteSummary do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :point_count, 1, type: :int32, json_name: "pointCount"
  field :feature_count, 2, type: :int32, json_name: "featureCount"
  field :distance, 3, type: :int32
  field :elapsed_time, 4, type: :int32, json_name: "elapsedTime"
end

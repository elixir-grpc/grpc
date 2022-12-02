defmodule Routeguide.Point do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :latitude, 1, type: :int32
  field :longitude, 2, type: :int32
end

defmodule Routeguide.Rectangle do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :lo, 1, type: Routeguide.Point
  field :hi, 2, type: Routeguide.Point
end

defmodule Routeguide.Feature do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :location, 2, type: Routeguide.Point
end

defmodule Routeguide.RouteNote do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :location, 1, type: Routeguide.Point
  field :message, 2, type: :string
end

defmodule Routeguide.RouteSummary do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :point_count, 1, type: :int32, json_name: "pointCount"
  field :feature_count, 2, type: :int32, json_name: "featureCount"
  field :distance, 3, type: :int32
  field :elapsed_time, 4, type: :int32, json_name: "elapsedTime"
end

defmodule Routeguide.RouteGuide.Service do
  @moduledoc false
  use GRPC.Service, name: "routeguide.RouteGuide", protoc_gen_elixir_version: "0.11.0"

  rpc :GetFeature, Routeguide.Point, Routeguide.Feature

  rpc :ListFeatures, Routeguide.Rectangle, stream(Routeguide.Feature)

  rpc :RecordRoute, stream(Routeguide.Point), Routeguide.RouteSummary

  rpc :RouteChat, stream(Routeguide.RouteNote), stream(Routeguide.RouteNote)
end

defmodule Routeguide.RouteGuide.Stub do
  @moduledoc false
  use GRPC.Stub, service: Routeguide.RouteGuide.Service
end

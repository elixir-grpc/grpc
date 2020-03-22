defmodule Routeguide.Point do
  use Protobuf

  @type t :: %__MODULE__{
          latitude: integer,
          longitude: integer
        }
  defstruct [:latitude, :longitude]

  field :latitude, 1, optional: true, type: :int32
  field :longitude, 2, optional: true, type: :int32
end

defmodule Routeguide.Rectangle do
  use Protobuf

  @type t :: %__MODULE__{
          lo: Routeguide.Point.t(),
          hi: Routeguide.Point.t()
        }
  defstruct [:lo, :hi]

  field :lo, 1, optional: true, type: Routeguide.Point
  field :hi, 2, optional: true, type: Routeguide.Point
end

defmodule Routeguide.Feature do
  use Protobuf

  @type t :: %__MODULE__{
          name: String.t(),
          location: Routeguide.Point.t()
        }
  defstruct [:name, :location]

  field :name, 1, optional: true, type: :string
  field :location, 2, optional: true, type: Routeguide.Point
end

defmodule Routeguide.RouteNote do
  use Protobuf

  @type t :: %__MODULE__{
          location: Routeguide.Point.t(),
          message: String.t()
        }
  defstruct [:location, :message]

  field :location, 1, optional: true, type: Routeguide.Point
  field :message, 2, optional: true, type: :string
end

defmodule Routeguide.RouteSummary do
  use Protobuf

  @type t :: %__MODULE__{
          point_count: integer,
          feature_count: integer,
          distance: integer,
          elapsed_time: integer
        }
  defstruct [:point_count, :feature_count, :distance, :elapsed_time]

  field :point_count, 1, optional: true, type: :int32
  field :feature_count, 2, optional: true, type: :int32
  field :distance, 3, optional: true, type: :int32
  field :elapsed_time, 4, optional: true, type: :int32
end

defmodule Routeguide.RouteGuide.Service do
  use GRPC.Service, name: "routeguide.RouteGuide"

  rpc :GetFeature, Routeguide.Point, Routeguide.Feature
  rpc :ListFeatures, Routeguide.Rectangle, stream(Routeguide.Feature)
  rpc :RecordRoute, stream(Routeguide.Point), Routeguide.RouteSummary
  rpc :RouteChat, stream(Routeguide.RouteNote), stream(Routeguide.RouteNote)
  rpc :AsyncRouteChat, stream(Routeguide.RouteNote), stream(Routeguide.RouteNote)
end

defmodule Routeguide.RouteGuide.Stub do
  use GRPC.Stub, service: Routeguide.RouteGuide.Service
end

defmodule RouteguideTranscode.RouteGuide.Service do
  @moduledoc false

  use GRPC.Service, name: "routeguide_transcode.RouteGuide", protoc_gen_elixir_version: "0.11.0"

  rpc(:GetFeature, RouteguideTranscode.Point, RouteguideTranscode.Feature, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/features/{latitude}/{longitude}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:ListFeatures, RouteguideTranscode.Rectangle, stream(RouteguideTranscode.Feature), %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/features"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:CreateFeature, RouteguideTranscode.Point, RouteguideTranscode.Feature, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:post, "/v1/features"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:RecordRoute, stream(RouteguideTranscode.Point), RouteguideTranscode.RouteSummary, %{})

  rpc(
    :RouteChat,
    stream(RouteguideTranscode.RouteNote),
    stream(RouteguideTranscode.RouteNote),
    %{}
  )
end

defmodule RouteguideTranscode.RouteGuide.Stub do
  @moduledoc false

  use GRPC.Stub, service: RouteguideTranscode.RouteGuide.Service
end

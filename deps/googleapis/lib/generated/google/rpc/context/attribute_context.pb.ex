defmodule Google.Rpc.Context.AttributeContext.Peer.LabelsEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Peer do
  @moduledoc """
  This message defines attributes for a node that handles a network request.
  The node can be either a service or an application that sends, forwards,
  or receives the request. Service peers should fill in
  `principal` and `labels` as appropriate.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :ip, 1, type: :string
  field :port, 2, type: :int64

  field :labels, 6,
    repeated: true,
    type: Google.Rpc.Context.AttributeContext.Peer.LabelsEntry,
    map: true

  field :principal, 7, type: :string
  field :region_code, 8, type: :string, json_name: "regionCode"
end

defmodule Google.Rpc.Context.AttributeContext.Api do
  @moduledoc """
  This message defines attributes associated with API operations, such as
  a network API request. The terminology is based on the conventions used
  by Google APIs, Istio, and OpenAPI.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :service, 1, type: :string
  field :operation, 2, type: :string
  field :protocol, 3, type: :string
  field :version, 4, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Auth do
  @moduledoc """
  This message defines request authentication attributes. Terminology is
  based on the JSON Web Token (JWT) standard, but the terms also
  correlate to concepts in other standards.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :principal, 1, type: :string
  field :audiences, 2, repeated: true, type: :string
  field :presenter, 3, type: :string
  field :claims, 4, type: Google.Protobuf.Struct
  field :access_levels, 5, repeated: true, type: :string, json_name: "accessLevels"
end

defmodule Google.Rpc.Context.AttributeContext.Request.HeadersEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Request do
  @moduledoc """
  This message defines attributes for an HTTP request. If the actual
  request is not an HTTP request, the runtime system should try to map
  the actual request to an equivalent HTTP request.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :id, 1, type: :string
  field :method, 2, type: :string

  field :headers, 3,
    repeated: true,
    type: Google.Rpc.Context.AttributeContext.Request.HeadersEntry,
    map: true

  field :path, 4, type: :string
  field :host, 5, type: :string
  field :scheme, 6, type: :string
  field :query, 7, type: :string
  field :time, 9, type: Google.Protobuf.Timestamp
  field :size, 10, type: :int64
  field :protocol, 11, type: :string
  field :reason, 12, type: :string
  field :auth, 13, type: Google.Rpc.Context.AttributeContext.Auth
end

defmodule Google.Rpc.Context.AttributeContext.Response.HeadersEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Response do
  @moduledoc """
  This message defines attributes for a typical network response. It
  generally models semantics of an HTTP response.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :code, 1, type: :int64
  field :size, 2, type: :int64

  field :headers, 3,
    repeated: true,
    type: Google.Rpc.Context.AttributeContext.Response.HeadersEntry,
    map: true

  field :time, 4, type: Google.Protobuf.Timestamp
  field :backend_latency, 5, type: Google.Protobuf.Duration, json_name: "backendLatency"
end

defmodule Google.Rpc.Context.AttributeContext.Resource.LabelsEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Resource.AnnotationsEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.Context.AttributeContext.Resource do
  @moduledoc """
  This message defines core attributes for a resource. A resource is an
  addressable (named) entity provided by the destination service. For
  example, a file stored on a network storage service.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :service, 1, type: :string
  field :name, 2, type: :string
  field :type, 3, type: :string

  field :labels, 4,
    repeated: true,
    type: Google.Rpc.Context.AttributeContext.Resource.LabelsEntry,
    map: true

  field :uid, 5, type: :string

  field :annotations, 6,
    repeated: true,
    type: Google.Rpc.Context.AttributeContext.Resource.AnnotationsEntry,
    map: true

  field :display_name, 7, type: :string, json_name: "displayName"
  field :create_time, 8, type: Google.Protobuf.Timestamp, json_name: "createTime"
  field :update_time, 9, type: Google.Protobuf.Timestamp, json_name: "updateTime"
  field :delete_time, 10, type: Google.Protobuf.Timestamp, json_name: "deleteTime"
  field :etag, 11, type: :string
  field :location, 12, type: :string
end

defmodule Google.Rpc.Context.AttributeContext do
  @moduledoc """
  This message defines the standard attribute vocabulary for Google APIs.

  An attribute is a piece of metadata that describes an activity on a network
  service. For example, the size of an HTTP request, or the status code of
  an HTTP response.

  Each attribute has a type and a name, which is logically defined as
  a proto message field in `AttributeContext`. The field type becomes the
  attribute type, and the field path becomes the attribute name. For example,
  the attribute `source.ip` maps to field `AttributeContext.source.ip`.

  This message definition is guaranteed not to have any wire breaking change.
  So you can use it directly for passing attributes across different systems.

  NOTE: Different system may generate different subset of attributes. Please
  verify the system specification before relying on an attribute generated
  a system.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :origin, 7, type: Google.Rpc.Context.AttributeContext.Peer
  field :source, 1, type: Google.Rpc.Context.AttributeContext.Peer
  field :destination, 2, type: Google.Rpc.Context.AttributeContext.Peer
  field :request, 3, type: Google.Rpc.Context.AttributeContext.Request
  field :response, 4, type: Google.Rpc.Context.AttributeContext.Response
  field :resource, 5, type: Google.Rpc.Context.AttributeContext.Resource
  field :api, 6, type: Google.Rpc.Context.AttributeContext.Api
  field :extensions, 8, repeated: true, type: Google.Protobuf.Any
end

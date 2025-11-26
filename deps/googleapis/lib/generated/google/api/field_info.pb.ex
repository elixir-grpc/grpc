defmodule Google.Api.FieldInfo.Format do
  @moduledoc """
  The standard format of a field value. The supported formats are all backed
  by either an RFC defined by the IETF or a Google-defined AIP.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :FORMAT_UNSPECIFIED, 0
  field :UUID4, 1
  field :IPV4, 2
  field :IPV6, 3
  field :IPV4_OR_IPV6, 4
end

defmodule Google.Api.FieldInfo do
  @moduledoc """
  Rich semantic information of an API field beyond basic typing.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :format, 1, type: Google.Api.FieldInfo.Format, enum: true

  field :referenced_types, 2,
    repeated: true,
    type: Google.Api.TypeReference,
    json_name: "referencedTypes"
end

defmodule Google.Api.TypeReference do
  @moduledoc """
  A reference to a message type, for use in [FieldInfo][google.api.FieldInfo].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type_name, 1, type: :string, json_name: "typeName"
end

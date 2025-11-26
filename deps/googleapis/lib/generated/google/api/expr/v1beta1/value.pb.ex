defmodule Google.Api.Expr.V1beta1.Value do
  @moduledoc """
  Represents a CEL value.

  This is similar to `google.protobuf.Value`, but can represent CEL's full
  range of values.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :kind, 0

  field :null_value, 1,
    type: Google.Protobuf.NullValue,
    json_name: "nullValue",
    enum: true,
    oneof: 0

  field :bool_value, 2, type: :bool, json_name: "boolValue", oneof: 0
  field :int64_value, 3, type: :int64, json_name: "int64Value", oneof: 0
  field :uint64_value, 4, type: :uint64, json_name: "uint64Value", oneof: 0
  field :double_value, 5, type: :double, json_name: "doubleValue", oneof: 0
  field :string_value, 6, type: :string, json_name: "stringValue", oneof: 0
  field :bytes_value, 7, type: :bytes, json_name: "bytesValue", oneof: 0
  field :enum_value, 9, type: Google.Api.Expr.V1beta1.EnumValue, json_name: "enumValue", oneof: 0
  field :object_value, 10, type: Google.Protobuf.Any, json_name: "objectValue", oneof: 0
  field :map_value, 11, type: Google.Api.Expr.V1beta1.MapValue, json_name: "mapValue", oneof: 0
  field :list_value, 12, type: Google.Api.Expr.V1beta1.ListValue, json_name: "listValue", oneof: 0
  field :type_value, 15, type: :string, json_name: "typeValue", oneof: 0
end

defmodule Google.Api.Expr.V1beta1.EnumValue do
  @moduledoc """
  An enum value.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: :string
  field :value, 2, type: :int32
end

defmodule Google.Api.Expr.V1beta1.ListValue do
  @moduledoc """
  A list.

  Wrapped in a message so 'not set' and empty can be differentiated, which is
  required for use in a 'oneof'.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :values, 1, repeated: true, type: Google.Api.Expr.V1beta1.Value
end

defmodule Google.Api.Expr.V1beta1.MapValue.Entry do
  @moduledoc """
  An entry in the map.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: Google.Api.Expr.V1beta1.Value
  field :value, 2, type: Google.Api.Expr.V1beta1.Value
end

defmodule Google.Api.Expr.V1beta1.MapValue do
  @moduledoc """
  A map.

  Wrapped in a message so 'not set' and empty can be differentiated, which is
  required for use in a 'oneof'.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :entries, 1, repeated: true, type: Google.Api.Expr.V1beta1.MapValue.Entry
end

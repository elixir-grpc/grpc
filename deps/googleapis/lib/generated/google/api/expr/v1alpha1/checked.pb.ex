defmodule Google.Api.Expr.V1alpha1.Type.PrimitiveType do
  @moduledoc """
  CEL primitive types.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :PRIMITIVE_TYPE_UNSPECIFIED, 0
  field :BOOL, 1
  field :INT64, 2
  field :UINT64, 3
  field :DOUBLE, 4
  field :STRING, 5
  field :BYTES, 6
end

defmodule Google.Api.Expr.V1alpha1.Type.WellKnownType do
  @moduledoc """
  Well-known protobuf types treated with first-class support in CEL.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :WELL_KNOWN_TYPE_UNSPECIFIED, 0
  field :ANY, 1
  field :TIMESTAMP, 2
  field :DURATION, 3
end

defmodule Google.Api.Expr.V1alpha1.CheckedExpr.ReferenceMapEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :int64
  field :value, 2, type: Google.Api.Expr.V1alpha1.Reference
end

defmodule Google.Api.Expr.V1alpha1.CheckedExpr.TypeMapEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :int64
  field :value, 2, type: Google.Api.Expr.V1alpha1.Type
end

defmodule Google.Api.Expr.V1alpha1.CheckedExpr do
  @moduledoc """
  A CEL expression which has been successfully type checked.
  Protos for representing CEL declarations and typed checked expressions.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :reference_map, 2,
    repeated: true,
    type: Google.Api.Expr.V1alpha1.CheckedExpr.ReferenceMapEntry,
    json_name: "referenceMap",
    map: true

  field :type_map, 3,
    repeated: true,
    type: Google.Api.Expr.V1alpha1.CheckedExpr.TypeMapEntry,
    json_name: "typeMap",
    map: true

  field :source_info, 5, type: Google.Api.Expr.V1alpha1.SourceInfo, json_name: "sourceInfo"
  field :expr_version, 6, type: :string, json_name: "exprVersion"
  field :expr, 4, type: Google.Api.Expr.V1alpha1.Expr
end

defmodule Google.Api.Expr.V1alpha1.Type.ListType do
  @moduledoc """
  List type with typed elements, e.g. `list<example.proto.MyMessage>`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :elem_type, 1, type: Google.Api.Expr.V1alpha1.Type, json_name: "elemType"
end

defmodule Google.Api.Expr.V1alpha1.Type.MapType do
  @moduledoc """
  Map type with parameterized key and value types, e.g. `map<string, int>`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key_type, 1, type: Google.Api.Expr.V1alpha1.Type, json_name: "keyType"
  field :value_type, 2, type: Google.Api.Expr.V1alpha1.Type, json_name: "valueType"
end

defmodule Google.Api.Expr.V1alpha1.Type.FunctionType do
  @moduledoc """
  Function type with result and arg types.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :result_type, 1, type: Google.Api.Expr.V1alpha1.Type, json_name: "resultType"
  field :arg_types, 2, repeated: true, type: Google.Api.Expr.V1alpha1.Type, json_name: "argTypes"
end

defmodule Google.Api.Expr.V1alpha1.Type.AbstractType do
  @moduledoc """
  Application defined abstract type.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string

  field :parameter_types, 2,
    repeated: true,
    type: Google.Api.Expr.V1alpha1.Type,
    json_name: "parameterTypes"
end

defmodule Google.Api.Expr.V1alpha1.Type do
  @moduledoc """
  Represents a CEL type.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :type_kind, 0

  field :dyn, 1, type: Google.Protobuf.Empty, oneof: 0
  field :null, 2, type: Google.Protobuf.NullValue, enum: true, oneof: 0
  field :primitive, 3, type: Google.Api.Expr.V1alpha1.Type.PrimitiveType, enum: true, oneof: 0
  field :wrapper, 4, type: Google.Api.Expr.V1alpha1.Type.PrimitiveType, enum: true, oneof: 0

  field :well_known, 5,
    type: Google.Api.Expr.V1alpha1.Type.WellKnownType,
    json_name: "wellKnown",
    enum: true,
    oneof: 0

  field :list_type, 6,
    type: Google.Api.Expr.V1alpha1.Type.ListType,
    json_name: "listType",
    oneof: 0

  field :map_type, 7, type: Google.Api.Expr.V1alpha1.Type.MapType, json_name: "mapType", oneof: 0
  field :function, 8, type: Google.Api.Expr.V1alpha1.Type.FunctionType, oneof: 0
  field :message_type, 9, type: :string, json_name: "messageType", oneof: 0
  field :type_param, 10, type: :string, json_name: "typeParam", oneof: 0
  field :type, 11, type: Google.Api.Expr.V1alpha1.Type, oneof: 0
  field :error, 12, type: Google.Protobuf.Empty, oneof: 0

  field :abstract_type, 14,
    type: Google.Api.Expr.V1alpha1.Type.AbstractType,
    json_name: "abstractType",
    oneof: 0
end

defmodule Google.Api.Expr.V1alpha1.Decl.IdentDecl do
  @moduledoc """
  Identifier declaration which specifies its type and optional `Expr` value.

  An identifier without a value is a declaration that must be provided at
  evaluation time. An identifier with a value should resolve to a constant,
  but may be used in conjunction with other identifiers bound at evaluation
  time.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: Google.Api.Expr.V1alpha1.Type
  field :value, 2, type: Google.Api.Expr.V1alpha1.Constant
  field :doc, 3, type: :string
end

defmodule Google.Api.Expr.V1alpha1.Decl.FunctionDecl.Overload do
  @moduledoc """
  An overload indicates a function's parameter types and return type, and
  may optionally include a function body described in terms of
  [Expr][google.api.expr.v1alpha1.Expr] values.

  Functions overloads are declared in either a function or method
  call-style. For methods, the `params[0]` is the expected type of the
  target receiver.

  Overloads must have non-overlapping argument types after erasure of all
  parameterized type variables (similar as type erasure in Java).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :overload_id, 1, type: :string, json_name: "overloadId"
  field :params, 2, repeated: true, type: Google.Api.Expr.V1alpha1.Type
  field :type_params, 3, repeated: true, type: :string, json_name: "typeParams"
  field :result_type, 4, type: Google.Api.Expr.V1alpha1.Type, json_name: "resultType"
  field :is_instance_function, 5, type: :bool, json_name: "isInstanceFunction"
  field :doc, 6, type: :string
end

defmodule Google.Api.Expr.V1alpha1.Decl.FunctionDecl do
  @moduledoc """
  Function declaration specifies one or more overloads which indicate the
  function's parameter types and return type.

  Functions have no observable side-effects (there may be side-effects like
  logging which are not observable from CEL).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :overloads, 1, repeated: true, type: Google.Api.Expr.V1alpha1.Decl.FunctionDecl.Overload
end

defmodule Google.Api.Expr.V1alpha1.Decl do
  @moduledoc """
  Represents a declaration of a named value or function.

  A declaration is part of the contract between the expression, the agent
  evaluating that expression, and the caller requesting evaluation.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :decl_kind, 0

  field :name, 1, type: :string
  field :ident, 2, type: Google.Api.Expr.V1alpha1.Decl.IdentDecl, oneof: 0
  field :function, 3, type: Google.Api.Expr.V1alpha1.Decl.FunctionDecl, oneof: 0
end

defmodule Google.Api.Expr.V1alpha1.Reference do
  @moduledoc """
  Describes a resolved reference to a declaration.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
  field :overload_id, 3, repeated: true, type: :string, json_name: "overloadId"
  field :value, 4, type: Google.Api.Expr.V1alpha1.Constant
end

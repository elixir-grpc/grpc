defmodule Google.Api.Expr.V1beta1.ParsedExpr do
  @moduledoc """
  An expression together with source information as returned by the parser.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :expr, 2, type: Google.Api.Expr.V1beta1.Expr
  field :source_info, 3, type: Google.Api.Expr.V1beta1.SourceInfo, json_name: "sourceInfo"
  field :syntax_version, 4, type: :string, json_name: "syntaxVersion"
end

defmodule Google.Api.Expr.V1beta1.Expr.Ident do
  @moduledoc """
  An identifier expression. e.g. `request`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Google.Api.Expr.V1beta1.Expr.Select do
  @moduledoc """
  A field selection expression. e.g. `request.auth`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :operand, 1, type: Google.Api.Expr.V1beta1.Expr
  field :field, 2, type: :string
  field :test_only, 3, type: :bool, json_name: "testOnly"
end

defmodule Google.Api.Expr.V1beta1.Expr.Call do
  @moduledoc """
  A call expression, including calls to predefined functions and operators.

  For example, `value == 10`, `size(map_value)`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :target, 1, type: Google.Api.Expr.V1beta1.Expr
  field :function, 2, type: :string
  field :args, 3, repeated: true, type: Google.Api.Expr.V1beta1.Expr
end

defmodule Google.Api.Expr.V1beta1.Expr.CreateList do
  @moduledoc """
  A list creation expression.

  Lists may either be homogenous, e.g. `[1, 2, 3]`, or heterogenous, e.g.
  `dyn([1, 'hello', 2.0])`
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :elements, 1, repeated: true, type: Google.Api.Expr.V1beta1.Expr
end

defmodule Google.Api.Expr.V1beta1.Expr.CreateStruct.Entry do
  @moduledoc """
  Represents an entry.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :key_kind, 0

  field :id, 1, type: :int32
  field :field_key, 2, type: :string, json_name: "fieldKey", oneof: 0
  field :map_key, 3, type: Google.Api.Expr.V1beta1.Expr, json_name: "mapKey", oneof: 0
  field :value, 4, type: Google.Api.Expr.V1beta1.Expr
end

defmodule Google.Api.Expr.V1beta1.Expr.CreateStruct do
  @moduledoc """
  A map or message creation expression.

  Maps are constructed as `{'key_name': 'value'}`. Message construction is
  similar, but prefixed with a type name and composed of field ids:
  `types.MyType{field_id: 'value'}`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: :string
  field :entries, 2, repeated: true, type: Google.Api.Expr.V1beta1.Expr.CreateStruct.Entry
end

defmodule Google.Api.Expr.V1beta1.Expr.Comprehension do
  @moduledoc """
  A comprehension expression applied to a list or map.

  Comprehensions are not part of the core syntax, but enabled with macros.
  A macro matches a specific call signature within a parsed AST and replaces
  the call with an alternate AST block. Macro expansion happens at parse
  time.

  The following macros are supported within CEL:

  Aggregate type macros may be applied to all elements in a list or all keys
  in a map:

  *  `all`, `exists`, `exists_one` -  test a predicate expression against
     the inputs and return `true` if the predicate is satisfied for all,
     any, or only one value `list.all(x, x < 10)`.
  *  `filter` - test a predicate expression against the inputs and return
     the subset of elements which satisfy the predicate:
     `payments.filter(p, p > 1000)`.
  *  `map` - apply an expression to all elements in the input and return the
     output aggregate type: `[1, 2, 3].map(i, i * i)`.

  The `has(m.x)` macro tests whether the property `x` is present in struct
  `m`. The semantics of this macro depend on the type of `m`. For proto2
  messages `has(m.x)` is defined as 'defined, but not set`. For proto3, the
  macro tests whether the property is set to its default. For map and struct
  types, the macro tests whether the property `x` is defined on `m`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :iter_var, 1, type: :string, json_name: "iterVar"
  field :iter_range, 2, type: Google.Api.Expr.V1beta1.Expr, json_name: "iterRange"
  field :accu_var, 3, type: :string, json_name: "accuVar"
  field :accu_init, 4, type: Google.Api.Expr.V1beta1.Expr, json_name: "accuInit"
  field :loop_condition, 5, type: Google.Api.Expr.V1beta1.Expr, json_name: "loopCondition"
  field :loop_step, 6, type: Google.Api.Expr.V1beta1.Expr, json_name: "loopStep"
  field :result, 7, type: Google.Api.Expr.V1beta1.Expr
end

defmodule Google.Api.Expr.V1beta1.Expr do
  @moduledoc """
  An abstract representation of a common expression.

  Expressions are abstractly represented as a collection of identifiers,
  select statements, function calls, literals, and comprehensions. All
  operators with the exception of the '.' operator are modelled as function
  calls. This makes it easy to represent new operators into the existing AST.

  All references within expressions must resolve to a [Decl][google.api.expr.v1beta1.Decl] provided at
  type-check for an expression to be valid. A reference may either be a bare
  identifier `name` or a qualified identifier `google.api.name`. References
  may either refer to a value or a function declaration.

  For example, the expression `google.api.name.startsWith('expr')` references
  the declaration `google.api.name` within a [Expr.Select][google.api.expr.v1beta1.Expr.Select] expression, and
  the function declaration `startsWith`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :expr_kind, 0

  field :id, 2, type: :int32

  field :literal_expr, 3,
    type: Google.Api.Expr.V1beta1.Literal,
    json_name: "literalExpr",
    oneof: 0

  field :ident_expr, 4, type: Google.Api.Expr.V1beta1.Expr.Ident, json_name: "identExpr", oneof: 0

  field :select_expr, 5,
    type: Google.Api.Expr.V1beta1.Expr.Select,
    json_name: "selectExpr",
    oneof: 0

  field :call_expr, 6, type: Google.Api.Expr.V1beta1.Expr.Call, json_name: "callExpr", oneof: 0

  field :list_expr, 7,
    type: Google.Api.Expr.V1beta1.Expr.CreateList,
    json_name: "listExpr",
    oneof: 0

  field :struct_expr, 8,
    type: Google.Api.Expr.V1beta1.Expr.CreateStruct,
    json_name: "structExpr",
    oneof: 0

  field :comprehension_expr, 9,
    type: Google.Api.Expr.V1beta1.Expr.Comprehension,
    json_name: "comprehensionExpr",
    oneof: 0
end

defmodule Google.Api.Expr.V1beta1.Literal do
  @moduledoc """
  Represents a primitive literal.

  This is similar to the primitives supported in the well-known type
  `google.protobuf.Value`, but richer so it can represent CEL's full range of
  primitives.

  Lists and structs are not included as constants as these aggregate types may
  contain [Expr][google.api.expr.v1beta1.Expr] elements which require evaluation and are thus not constant.

  Examples of literals include: `"hello"`, `b'bytes'`, `1u`, `4.2`, `-2`,
  `true`, `null`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :constant_kind, 0

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
end

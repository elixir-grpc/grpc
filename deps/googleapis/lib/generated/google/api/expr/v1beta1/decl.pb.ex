defmodule Google.Api.Expr.V1beta1.Decl do
  @moduledoc """
  A declaration.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :kind, 0

  field :id, 1, type: :int32
  field :name, 2, type: :string
  field :doc, 3, type: :string
  field :ident, 4, type: Google.Api.Expr.V1beta1.IdentDecl, oneof: 0
  field :function, 5, type: Google.Api.Expr.V1beta1.FunctionDecl, oneof: 0
end

defmodule Google.Api.Expr.V1beta1.DeclType do
  @moduledoc """
  The declared type of a variable.

  Extends runtime type values with extra information used for type checking
  and dispatching.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :id, 1, type: :int32
  field :type, 2, type: :string

  field :type_params, 4,
    repeated: true,
    type: Google.Api.Expr.V1beta1.DeclType,
    json_name: "typeParams"
end

defmodule Google.Api.Expr.V1beta1.IdentDecl do
  @moduledoc """
  An identifier declaration.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 3, type: Google.Api.Expr.V1beta1.DeclType
  field :value, 4, type: Google.Api.Expr.V1beta1.Expr
end

defmodule Google.Api.Expr.V1beta1.FunctionDecl do
  @moduledoc """
  A function declaration.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :args, 1, repeated: true, type: Google.Api.Expr.V1beta1.IdentDecl
  field :return_type, 2, type: Google.Api.Expr.V1beta1.DeclType, json_name: "returnType"
  field :receiver_function, 3, type: :bool, json_name: "receiverFunction"
end

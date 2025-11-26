defmodule Google.Api.Expr.V1alpha1.EvalState.Result do
  @moduledoc """
  A single evalution result.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :expr, 1, type: :int64
  field :value, 2, type: :int64
end

defmodule Google.Api.Expr.V1alpha1.EvalState do
  @moduledoc """
  The state of an evaluation.

  Can represent an inital, partial, or completed state of evaluation.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :values, 1, repeated: true, type: Google.Api.Expr.V1alpha1.ExprValue
  field :results, 3, repeated: true, type: Google.Api.Expr.V1alpha1.EvalState.Result
end

defmodule Google.Api.Expr.V1alpha1.ExprValue do
  @moduledoc """
  The value of an evaluated expression.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :kind, 0

  field :value, 1, type: Google.Api.Expr.V1alpha1.Value, oneof: 0
  field :error, 2, type: Google.Api.Expr.V1alpha1.ErrorSet, oneof: 0
  field :unknown, 3, type: Google.Api.Expr.V1alpha1.UnknownSet, oneof: 0
end

defmodule Google.Api.Expr.V1alpha1.ErrorSet do
  @moduledoc """
  A set of errors.

  The errors included depend on the context. See `ExprValue.error`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :errors, 1, repeated: true, type: Google.Rpc.Status
end

defmodule Google.Api.Expr.V1alpha1.UnknownSet do
  @moduledoc """
  A set of expressions for which the value is unknown.

  The unknowns included depend on the context. See `ExprValue.unknown`.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :exprs, 1, repeated: true, type: :int64
end

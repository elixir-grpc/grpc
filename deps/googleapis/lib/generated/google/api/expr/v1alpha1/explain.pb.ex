defmodule Google.Api.Expr.V1alpha1.Explain.ExprStep do
  @moduledoc """
  ID and value index of one step.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :id, 1, type: :int64
  field :value_index, 2, type: :int32, json_name: "valueIndex"
end

defmodule Google.Api.Expr.V1alpha1.Explain do
  @moduledoc """
  Values of intermediate expressions produced when evaluating expression.
  Deprecated, use `EvalState` instead.
  """

  use Protobuf, deprecated: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :values, 1, repeated: true, type: Google.Api.Expr.V1alpha1.Value

  field :expr_steps, 2,
    repeated: true,
    type: Google.Api.Expr.V1alpha1.Explain.ExprStep,
    json_name: "exprSteps"
end

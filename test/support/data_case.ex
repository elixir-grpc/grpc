defmodule GRPC.DataCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GRPC.Factory
    end
  end
end

defmodule GRPC.Client.DataCase do
  @moduledoc """
  This module defines the test case to be used by
  tests for grpc_client.
  """
  use ExUnit.CaseTemplate

  using do
    quote do
      import GRPC.Factory
    end
  end
end

defmodule GRPC.Test.ClientAdapter do
  def connect_insecurely(_) do
    {:ok, %{name: "Test.ClientAdapter"}}
  end
end

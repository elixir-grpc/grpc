defmodule GRPC do
  @moduledoc """
    GRPC
  """

  def version, do: GRPC.Mixfile.project[:version]
  def user_agent, do: "grpc-elixir/#{version()}"
end

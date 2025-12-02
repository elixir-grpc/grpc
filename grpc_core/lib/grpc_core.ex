defmodule GRPC.Core do
  @moduledoc false

  @version GRPC.Core.MixProject.project()[:version]

  @doc """
  Returns version of this project.
  """
  def version, do: @version

  @doc false
  def user_agent, do: "grpc-elixir/#{version()}"
end

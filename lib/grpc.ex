defmodule GRPC do
  @version GRPC.Mixfile.project()[:version]

  @doc """
  Returns version of this project.
  """
  def version, do: @version

  @doc false
  def user_agent, do: "grpc-elixir/#{version()}"
end

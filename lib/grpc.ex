defmodule GRPC do
  @version GRPC.Mixfile.project()[:version]

  @doc """
  Returns version of this project.
  """
  def version, do: @version

  @doc """
  Returns user agent used by HTTP/2.
  """
  def user_agent, do: "grpc-elixir/#{version()}"
end

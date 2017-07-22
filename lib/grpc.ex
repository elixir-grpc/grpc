defmodule GRPC do
  @moduledoc false
  alias GRPC.Mixfile
  @doc """
  Returns version of this project.
  """
  def version, do: Mixfile.project[:version]

  @doc """
  Returns user agent used by HTTP/2.
  """
  def user_agent, do: "grpc-elixir/#{version()}"
end

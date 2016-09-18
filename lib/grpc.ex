defmodule GRPC do
  def version, do: GRPC.Mixfile.project[:version]
  def user_agent, do: "grpc-elixir/#{version}"
end

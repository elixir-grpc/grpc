defmodule GRPC.Server.HTTPTranscode do
  @spec path(term()) :: String.t()
  def path(%{pattern: {_method, path}}) do
    path
  end

  @spec method(term()) :: String.t()
  def method(%{pattern: {method, _path}}) do
    method
  end
end

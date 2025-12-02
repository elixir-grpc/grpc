defmodule GRPC.Client.Resolver.DNS.Adapter do
  @moduledoc """
  Adapter to resolve DNS (A and TXT).
  """

  @callback lookup(String.t(), :a | :txt) ::
              {:ok, [tuple() | String.t()]} | {:error, term()}

  def lookup(name, type) do
    :inet_res.lookup(String.to_charlist(name), :in, type)
  end
end

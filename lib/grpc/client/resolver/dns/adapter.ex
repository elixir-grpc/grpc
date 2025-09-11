defmodule GRPC.Client.Resolver.DNS.Adapter do
  @moduledoc """
  Adapter to resolve DNS (A and TXT).
  """

  @callback lookup(charlist(), :a | :txt) ::
              {:ok, [tuple() | String.t()]} | {:error, term()}

  def lookup(name, type) do
    :inet_res.lookup(name, :in, type)
  end
end

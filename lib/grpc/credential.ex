defmodule GRPC.Credential do
  @moduledoc """
  Stores credentials for authentication. It can be used to establish secure connections
  by passed to `GRPC.Stub.connect/2` as an argument.

  ## Examples

      iex> cred = GRPC.Credential.new(ssl: [cacertfile: ca_path])
      iex> GRPC.Stub.connect("localhost:10000", cred: cred)
  """

  @type t :: %__MODULE__{ssl: [:ssl.tls_option()]}
  defstruct [:ssl]

  @doc """
  Creates credential.
  """
  def new(opts) do
    %__MODULE__{ssl: Keyword.get(opts, :ssl, [])}
  end
end

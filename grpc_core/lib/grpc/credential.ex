defmodule GRPC.Credential do
  @moduledoc """
  Stores credentials for authentication.

  It can be used to establish secure connections
  by passed to `GRPC.Stub.connect/2` as an argument.

  Some client and server adapter implementations may
  choose to let request options override some of the
  configuration here, but this is left as a choice
  for each adapter.

  ## Examples

      iex> cred = GRPC.Credential.new(ssl: [cacertfile: ca_path])
      iex> GRPC.Stub.connect("localhost:10000", cred: cred)
  """

  @type t :: %__MODULE__{ssl: [:ssl.tls_option()]}
  defstruct ssl: []

  @doc """
  Creates credential.
  """
  def new(opts) do
    opts = Keyword.validate!(opts, [:ssl])
    %__MODULE__{ssl: opts[:ssl] || []}
  end
end

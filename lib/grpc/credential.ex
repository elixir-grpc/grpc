defmodule GRPC.Credential do
  @moduledoc """
  Stores credentials for authentication.
  """

  @type t :: %__MODULE__{ssl: [:ssl.ssl_option]}
  defstruct [:ssl]

  @doc """
  Creates credential.
  """
  def new(opts) do
    %__MODULE__{ssl: Keyword.get(opts, :ssl, [])}
  end
end

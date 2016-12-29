defmodule GRPC.Credential.TLS do
  @moduledoc """
  Stores TLS credential.
  """

  @type t :: %__MODULE__{cert_path: String.t, key_path: String.t}
  defstruct [:cert_path, :key_path]
end

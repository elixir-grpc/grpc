defmodule GRPC.Credential.ServerTLS do
  @moduledoc """
  Stores server TLS credential.
  """

  @type t :: %__MODULE__{cert_path: String.t, key_path: String.t}
  defstruct [:cert_path, :key_path]
end

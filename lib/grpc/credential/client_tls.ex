defmodule GRPC.Credential.ClientTLS do
  @moduledoc """
  Stores client TLS credential.
  """

  @type t :: %__MODULE__{ca_path: String.t}
  defstruct [:ca_path]
end

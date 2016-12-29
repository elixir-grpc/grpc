defmodule GRPC.Credential do
  @moduledoc """
  Stores credentials for authentication.
  """

  @type t :: %__MODULE__{tls: GRPC.Credential.TLS.t}
  defstruct [:tls]

  @doc """
  Creates TLS credential
  """
  def server_tls(cert_path, key_path) do
    tls = %GRPC.Credential.TLS{cert_path: cert_path, key_path: key_path}
    %__MODULE__{tls: tls}
  end
end

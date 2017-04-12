defmodule GRPC.Credential do
  @moduledoc """
  Stores credentials for authentication.
  """

  @type t :: %__MODULE__{tls: GRPC.Credential.ClientTLS.t | GRPC.Credential.ServerTLS.t}
  defstruct [:tls]

  @doc """
  Creates server TLS credential.
  """
  def server_tls(cert_path, key_path) do
    tls = %GRPC.Credential.ServerTLS{cert_path: cert_path, key_path: key_path}
    %__MODULE__{tls: tls}
  end

  @doc """
  Creates client TLS credential.
  """
  def client_tls(ca_path) do
    tls = %GRPC.Credential.ClientTLS{ca_path: ca_path}
    %__MODULE__{tls: tls}
  end
end

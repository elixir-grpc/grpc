defmodule GRPC.Client.Resolver.Unix do
  @moduledoc """
  Resolver for gRPC clients connecting via Unix Domain Sockets (UDS).

  This resolver handles target strings using the `unix` URI scheme, which
  allows a gRPC client to connect to a server via a Unix socket path. Unix
  domain sockets are supported on Unix systems only.

  ## Target format

      unix:///absolute/path/to/socket

  - The scheme **must** be `unix`.
  - The path must be absolute (`/var/run/my.sock`).
  - The port is not used in Unix sockets; `:port` will be `nil`.
  - The socket type is indicated via `:socket => :unix`.

  ## Example

      target = "unix:///var/run/my_grpc.sock"

      {:ok, %{addresses: addresses, service_config: nil}} =
        GRPC.Client.Resolver.Unix.resolve(target)

      addresses
      # => [%{address: "/var/run/my_grpc.sock", port: nil, socket: :unix}]

  This resolver always returns `nil` for the service config, as Unix
  sockets do not provide DNS TXT records or xDS configuration.

  See the gRPC naming documentation for more information on URI-based
  resolution: https://github.com/grpc/grpc/blob/master/doc/naming.md
  """

  @behaviour GRPC.Client.Resolver

  @impl GRPC.Client.Resolver
  def resolve(target) do
    # E.g.: "unix:///var/run/my.sock"
    uri = URI.parse(target)
    path = uri.path

    {:ok, %{addresses: [%{address: {:local, path}, port: 0, socket: :unix}], service_config: nil}}
  end
end

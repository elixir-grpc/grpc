defmodule GRPC.Channel do
  @moduledoc """
  A struct to store the connection data, which should be passed to
  RPC functions as the first argument:

      Greeter.Stub.say_hello(channel, request)

  ## Fields

    * `:host` - server's host to connect
    * `:port` - server's port to connect
    * `:scheme` - scheme of connection, like `http`
    * `:cred` - credentials used for authentication
    * `:adapter` - a client adapter module, like `GRPC.Adapter.Gun`
    * `:adapter_payload` - payload the adapter uses
  """

  @type t :: %__MODULE__{
          host: String.t(),
          port: non_neg_integer,
          scheme: String.t(),
          cred: GRPC.Credential.t(),
          adapter: atom,
          adapter_payload: any
        }
  defstruct host: nil, port: nil, scheme: nil, cred: nil, adapter: nil, adapter_payload: nil
end

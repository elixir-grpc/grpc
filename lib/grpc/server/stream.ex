defmodule GRPC.Server.Stream do
  @moduledoc """
  Defines a stream struct used in a request handled by the server.

  ## Fields

    * `:server` - user defined gRPC server module
    * `:marshal` - a function encoding the reply
    * `:unmarshal` - a function decoding the request
    * `:adapter` - a server adapter module, like `GRPC.Adapter.Cowboy`
    * `:payload` - the payload needed by the adapter
    * `:metadata` - metadata sent by client
  """

  defstruct [:server, :marshal, :unmarshal, :payload, :adapter, :metadata]

  @type marshal :: (struct -> binary)
  @type unmarshal :: (binary -> struct)
  @type t :: %__MODULE__{
          server: atom,
          marshal: marshal,
          unmarshal: unmarshal,
          payload: any,
          adapter: atom,
          metadata: list
        }
end

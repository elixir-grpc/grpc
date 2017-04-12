defmodule GRPC.Server.Stream do
  @moduledoc """
  Defines a stream struct used in a request handled by the server.

  ## Fields

    * `:server` - user defined gRPC server module
    * `:marshal` - a function encoding the reply
    * `:unmarshal` - a function decoding the request
    * `:adapter` - a server adapter module, like `GRPC.Adapter.Cowboy`
    * `:payload` - the payload needed by the adapter
  """

  defstruct [:server, :marshal, :unmarshal, :payload, :adapter]

  @type marshal   :: (struct -> binary)
  @type unmarshal :: (binary -> struct)
  @type t :: %__MODULE__{server: atom, marshal: marshal, unmarshal: unmarshal,
                         payload: any, adapter: atom}
end

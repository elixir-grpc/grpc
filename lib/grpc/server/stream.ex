defmodule GRPC.Server.Stream do
  @moduledoc """
  A struct as an argument that servers get in rpc function definitions and use to handle headers,
  send streaming replies.

  Notice that you MUST use new stream returned by `GRPC.Server` as an argument to invoke next
  functions defined by `GRPC.Server`.

  ## Fields

    * `:server`    - user defined gRPC server module
    * `:marshal`   - a function encoding the reply
    * `:unmarshal` - a function decoding the request
    * `:adapter`   - a server adapter module, like `GRPC.Adapter.Cowboy`
    * `:payload`   - the payload needed by the adapter
    * `:local`     - local data initialized by user
  """

  defstruct [:server, :marshal, :unmarshal, :payload, :adapter, :local]

  @typep marshal :: (struct -> binary)
  @typep unmarshal :: (binary -> struct)
  @type t :: %__MODULE__{
          server: atom,
          marshal: marshal,
          unmarshal: unmarshal,
          payload: any,
          adapter: atom,
          local: any
        }
end

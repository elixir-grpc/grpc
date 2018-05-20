defmodule GRPC.Server.Stream do
  @moduledoc """
  A struct as an argument that servers get in rpc function definitions and use to handle headers,
  send streaming replies.

  Notice that you MUST use new stream returned by `GRPC.Server` as an argument to invoke next
  functions defined by `GRPC.Server`.

  ## Fields

    * `:server` - user defined gRPC server module
    * `:marshal` - a function encoding the reply
    * `:unmarshal` - a function decoding the request
    * `:adapter` - a server adapter module, like `GRPC.Adapter.Cowboy`
    * `:payload` - the payload needed by the adapter
  """

  defstruct [
    server: nil, service_name: nil, method_name: nil, grpc_type: nil, endpoint: nil, rpc: nil,
    marshal: nil, unmarshal: nil, payload: nil, adapter: nil,
    __interface__: %{send_reply: &__MODULE__.send_reply/2}]

  @typep marshal :: (struct -> binary)
  @typep unmarshal :: (binary -> struct)
  @type t :: %__MODULE__{
          server: atom,
          service_name: String.t,
          method_name: String.t,
          grpc_type: atom,
          endpoint: atom,
          rpc: tuple,
          marshal: marshal,
          unmarshal: unmarshal,
          payload: any,
          adapter: atom,
          __interface__: map
        }

  def send_reply(%{adapter: adapter, marshal: marshal} = stream, reply) do
    {:ok, data, _size} = reply |> marshal.() |> GRPC.Message.to_data(%{iolist: true})
    adapter.send_reply(stream.payload, data)
    stream
  end
end

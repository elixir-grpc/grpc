defmodule GRPC.Server.Stream do
  @moduledoc """
  A struct as an argument that servers get in rpc function definitions and use to handle headers,
  send streaming replies.

  Notice that you MUST use new stream returned by `GRPC.Server` as an argument to invoke next
  functions defined by `GRPC.Server`.

  ## Fields

    * `:server`            - user defined gRPC server module
    * `:adapter`           - a server adapter module, like `GRPC.Adapter.Cowboy`
    * `request_mod`        - the request module, or nil for untyped protocols
    * `response_mod`       - the response module, or nil for untyped protocols
    * `:codec`             - the codec
    * `:payload`           - the payload needed by the adapter
    * `:local`             - local data initialized by user
  """

  @type t :: %__MODULE__{
          server: atom,
          service_name: String.t(),
          method_name: String.t(),
          grpc_type: atom,
          endpoint: atom,
          rpc: tuple,
          request_mod: atom,
          response_mod: atom,
          codec: atom,
          payload: any,
          adapter: atom,
          local: any,
          __interface__: map
        }

  defstruct server: nil,
            service_name: nil,
            method_name: nil,
            grpc_type: nil,
            endpoint: nil,
            rpc: nil,
            request_mod: nil,
            response_mod: nil,
            codec: GRPC.Codec.Proto,
            payload: nil,
            adapter: nil,
            local: nil,
            __interface__: %{send_reply: &__MODULE__.send_reply/2}

  def send_reply(%{adapter: adapter, codec: codec} = stream, reply) do
    {:ok, data, _size} = reply |> codec.encode() |> GRPC.Message.to_data(%{iolist: true})
    adapter.send_reply(stream.payload, data)
    stream
  end
end

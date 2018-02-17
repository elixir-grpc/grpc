defmodule GRPC.Client.Stream do
  @moduledoc """
  Defines a stream struct used in requests sent by the client.

  ## Fields

    * `:channel` - `GRPC.Channel`, the channel established by client
    * `:payload` - data used by adapter in a request
    * `:path` - the request path to sent
    * `:marshal` - a function encoding the request
    * `:unmarshal` - a function decoding the reply
    * `:req_stream` - indicates if request is streaming
    * `:res_stream` - indicates if reply is streaming
  """

  @type marshal :: (struct -> binary)
  @type unmarshal :: (binary -> struct)
  @type stream_payload :: %{stream_id: :h2_connection.stream_id()}
  @type t :: %__MODULE__{
          channel: GRPC.Channel.t(),
          payload: stream_payload,
          path: String.t(),
          marshal: marshal,
          unmarshal: unmarshal,
          req_stream: boolean,
          res_stream: boolean
        }
  defstruct channel: nil,
            payload: %{},
            path: nil,
            marshal: nil,
            unmarshal: nil,
            req_stream: nil,
            res_stream: nil

  def put_payload(%{payload: payload} = stream, key, val) do
    payload = if payload, do: payload, else: %{}
    %{stream | payload: Map.put(payload, key, val)}
  end
end

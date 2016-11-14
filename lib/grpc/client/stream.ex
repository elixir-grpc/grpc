defmodule GRPC.Client.Stream do
  defstruct [channel: nil, payload: %{}, path: nil, marshal: nil, unmarshal: nil,
             req_stream: nil, res_stream: nil]

  @type marshal   :: (struct -> binary)
  @type unmarshal :: (struct -> binary)
  @type t :: %__MODULE__{channel: GRPC.Channel.t, payload: map, path: String.t,
                         marshal: marshal, unmarshal: unmarshal, req_stream: boolean,
                         res_stream: boolean}
end

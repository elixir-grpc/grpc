defmodule GRPC.Client.Stream do
  defstruct [channel: nil, payload: %{}, path: nil, marshal: nil, unmarshal: nil,
             req_stream: nil, res_stream: nil]
end

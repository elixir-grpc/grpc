defmodule GRPC.Server.Stream do
  defstruct [:server, :marshal, :unmarshal, :payload, :adapter]

  @type marshal   :: (struct -> binary)
  @type unmarshal :: (struct -> binary)
  @type t :: %__MODULE__{server: atom, marshal: marshal, unmarshal: unmarshal,
                         payload: any, adapter: atom}
end

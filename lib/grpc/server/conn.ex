defmodule GRPC.Server.Conn do
  defstruct [:server, :marshal, :unmarshal, :state]
end

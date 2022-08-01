if {:module, Mint} == Code.ensure_compiled(Mint) do
  defmodule GRPC.Client.Adapters.Mint do
    @moduledoc """
    Connection metadata that define the `adapter_payload` field for a Mint-controlled `GRPC.Channel`
    """
    defstruct [
      :conn,
      requests: %{},
      keep_alive_timer: %GRPC.Client.Adapters.Mint.KeepAliveTimer{}
    ]
  end
end

defmodule GRPC.Server.Adapter do
  @moduledoc """
  HTTP server adapter for GRPC.
  """

  @callback start(
              atom(),
              %{String.t() => [module()]},
              port :: non_neg_integer(),
              opts :: keyword()
            ) ::
              {atom(), any(), non_neg_integer()}

  @callback stop(atom(), %{String.t() => [module()]}) :: :ok | {:error, :not_found}

  @callback send_reply(Handler.state(), content :: binary(), opts :: keyword()) :: any()

  @callback send_headers(Handler.state(), headers :: map()) :: any()
end

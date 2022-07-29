defmodule GRPC.Server.Adapter do
  @moduledoc """
  HTTP server adapter for GRPC.
  """

  alias GRPC.Server.Adapters.Cowboy.Handler

  @callback start(atom(), %{String.t() => [module()]}, non_neg_integer(), keyword()) ::
              {atom(), any(), non_neg_integer()}

  @callback stop(atom(), %{String.t() => [module()]}) :: :ok | {:error, :not_found}

  @callback send_reply(Handler.state(), binary(), keyword()) :: any()

  @callback send_headers(Handler.state(), map()) :: any()
end

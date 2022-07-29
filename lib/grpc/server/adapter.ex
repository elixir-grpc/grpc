defmodule GRPC.Server.Adapter do
  @moduledoc """
  HTTP server adapter for GRPC.
  """

  @type state :: %{
          pid: pid,
          handling_timer: reference | nil,
          resp_trailers: map,
          compressor: atom | nil,
          pending_reader: nil
        }

  @callback start(
              atom(),
              %{String.t() => [module()]},
              port :: non_neg_integer(),
              opts :: keyword()
            ) ::
              {atom(), any(), non_neg_integer()}

  @callback stop(atom(), %{String.t() => [module()]}) :: :ok | {:error, :not_found}

  @callback send_reply(state, content :: binary(), opts :: keyword()) :: any()

  @callback send_headers(state, headers :: map()) :: any()
end

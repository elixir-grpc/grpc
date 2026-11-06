defmodule GRPC.Client.Pool.Config do
  @moduledoc false

  defstruct [
    :pool_ref,
    :channel,
    :pool_size,
    :max_pool_overflow,
    :max_client_streams_per_connection,
    :adapter_opts,
    :health_check_enabled
  ]

  @type t :: %__MODULE__{
          pool_ref: reference(),
          channel: GRPC.Channel.t(),
          pool_size: non_neg_integer(),
          max_pool_overflow: non_neg_integer() | nil,
          max_client_streams_per_connection: non_neg_integer() | nil,
          adapter_opts: keyword(),
          health_check_enabled: boolean()
        }
end

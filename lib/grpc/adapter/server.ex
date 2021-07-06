defmodule GRPC.ServerAdapter do
  @moduledoc false

  @type opts :: Keyword.t()

  @type endpoint :: atom
  @type server_port :: non_neg_integer()
  @type servers_map :: %{String.t() => [module]}
  @callback start(endpoint, servers_map, server_port, opts) :: {atom, any, non_neg_integer}

  @callback stop(endpoint, servers_map) :: :ok | {:error, :not_found}

  @type stream :: GRPC.Server.Stream.t()

  @type state :: GRPC.Adapter.Cowboy.Handler.state()
  @type reply :: binary()
  @callback send_reply(state, reply, opts) :: any()

  @type headers :: map()
  @callback send_headers(state, headers) :: any()
end

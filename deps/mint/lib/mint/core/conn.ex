defmodule Mint.Core.Conn do
  @moduledoc false

  alias Mint.Types

  @type conn() :: term()

  @callback initiate(
              module(),
              Mint.Types.socket(),
              String.t(),
              :inet.port_number(),
              keyword()
            ) :: {:ok, conn()} | {:error, Types.error()}

  @callback open?(conn(), :read | :write) :: boolean()

  @callback close(conn()) :: {:ok, conn()}

  @callback request(
              conn(),
              method :: String.t(),
              path :: String.t(),
              Types.headers(),
              body :: iodata() | nil | :stream
            ) ::
              {:ok, conn(), Types.request_ref()}
              | {:error, conn(), Types.error()}

  @callback stream_request_body(
              conn(),
              Types.request_ref(),
              body_chunk :: iodata() | :eof | {:eof, trailer_headers :: Types.headers()}
            ) ::
              {:ok, conn()} | {:error, conn(), Types.error()}

  @callback stream(conn(), term()) ::
              {:ok, conn(), [Types.response()]}
              | {:error, conn(), Types.error(), [Types.response()]}
              | :unknown

  @callback open_request_count(conn()) :: non_neg_integer()

  @callback recv(conn(), byte_count :: non_neg_integer(), timeout()) ::
              {:ok, conn(), [Types.response()]}
              | {:error, conn(), Types.error(), [Types.response()]}

  @callback set_mode(conn(), :active | :passive) :: {:ok, conn()} | {:error, Types.error()}

  @callback controlling_process(conn(), pid()) :: {:ok, conn()} | {:error, Types.error()}

  @callback put_private(conn(), key :: atom(), value :: term()) :: conn()

  @callback get_private(conn(), key :: atom(), default_value :: term()) :: term()

  @callback delete_private(conn(), key :: atom()) :: conn()

  @callback get_socket(conn()) :: Mint.Types.socket()

  @callback get_proxy_headers(conn()) :: Mint.Types.headers()

  @callback put_proxy_headers(conn(), Mint.Types.headers()) :: conn()

  @callback put_log(conn(), boolean()) :: conn()
end

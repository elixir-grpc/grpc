if {:module, Mint} == Code.ensure_compiled(Mint) do
  defmodule GRPC.Client.Adapters.Mint do
    @moduledoc """
    `GRPC.Client.Adapter` implementation with `Mint`
    """

    alias GRPC.Client.Stream
    alias GRPC.Channel
    alias GRPC.Credential

    @behaviour GRPC.Client.Adapter

    @default_transport_opts [nodelay: true]
    @default_mint_opts [mode: :active, protocols: [:http2]]

    @doc """
    ## Options

      * `:keep_alive_interval` - the interval for HTTP2 keep-alive
      * `:keep_alive_timeout` - the timeout for HTTP2 keep-alive
      * `:transport_opts` - as defined by `Mint.HTTP.connect/4`.
      If the scheme is `"https"`, the SSL options given in `%Channel{cred: ...}` are
      merged here as default values.
    """
    def connect(
          %Channel{scheme: scheme, host: host, port: port, cred: cred} = channel,
          opts \\ []
        ) do
      {scheme, transport_opts} =
        case {scheme, cred} do
          {"https", %Credential{ssl: ssl}} when is_list(ssl) ->
            transport_opts = opts[:transport_opts] || []
            tls_opts = Keyword.merge(@default_transport_opts ++ ssl, transport_opts)
            {:https, tls_opts}

          {"http", _} ->
            {:http, opts[:transport_opts]}

          _ ->
            raise ArgumentError, "invalid scheme #{scheme}"
        end

      mint_opts =
        @default_mint_opts
        |> Keyword.merge(opts)
        |> Keyword.put(:transport_opts, transport_opts)

      case Mint.HTTP.connect(scheme, host, port, mint_opts) do
        {:ok, conn} ->
          {:ok,
           %{
             channel
             | adapter_payload: %Connection{
                 conn: conn,
                 keep_alive_timer: KeepAliveTimer.start(opts)
               }
           }}

        error ->
          error
      end
    end

    def disconnect(
          %Channel{
            adapter_payload:
              %Connection{conn: conn, keep_alive_timer: keep_alive_timer} = connection
          } = channel
        ) do
      {:ok, _} = Mint.HTTP.close(conn)

      :ok = close_requests(connection)

      {:ok,
       %{
         channel
         | adapter_payload: %{
             connection
             | conn: nil,
               requests: %{},
               keep_alive_timer: KeepAliveTimer.clear(keep_alive_timer)
           }
       }}
    end

    def send_request(%Stream{} = stream, contents, opts \\ []) do
      raise "not implemented"
    end

    def recv_headers(%Stream{} = stream, headers, opts \\ []) do
      raise "not implemented"
    end

    def recv_data_or_trailers(%Stream{} = stream, trailers_or_metadata, opts \\ []) do
      raise "not implemented"
    end
  end
end

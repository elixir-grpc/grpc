defmodule GRPC.Adapter.Chatterbox.Client do
  @moduledoc """
  A client(`GRPC.Channel`) adapter based on Chatterbox.

  Only one connection will be used for the same host and port. If server disconnects,
  it will try to reconnect before sending a request.
  """

  @type send_opts :: :h2_connection.send_opts()

  @spec connect(GRPC.Channel.t(), any) :: {:ok, GRPC.Channel.t()} | {:error, any}
  def connect(%{scheme: "https"} = channel, _), do: connect_secure(channel)
  def connect(channel, _), do: connect_insecure(channel)

  defp connect_secure(%{host: host, port: port, cred: %{ssl: ssl}} = channel) do
    do_connect(
      channel,
      {:client, :ssl, String.to_charlist(host), port, ssl, :chatterbox.settings(:client)}
    )
  end

  defp connect_insecure(%{host: host, port: port} = channel) do
    do_connect(
      channel,
      {:client, :gen_tcp, String.to_charlist(host), port, [], :chatterbox.settings(:client)}
    )
  end

  defp do_connect(channel, init_args) do
    pname = process_name(channel)

    case :gen_statem.start_link({:local, pname}, :h2_connection, init_args, []) do
      {:ok, _pid} ->
        {:ok, channel}

      {:error, {:already_started, _pid}} ->
        {:ok, channel}

      err = {:error, _} ->
        err

      _ ->
        {:error, "Unknown error!"}
    end
  end

  defp process_name(%{host: host, port: port, scheme: "https"}),
    do: :"grpc_chatter_client_ssl_#{host}:#{port}"

  defp process_name(%{host: host, port: port}), do: :"grpc_chatter_client_#{host}:#{port}"

  @spec unary(GRPC.Client.Stream.t(), struct, keyword) :: struct
  def unary(stream, message, opts) do
    {:ok, stream} = send_request(stream, message, opts)
    recv_end(stream, opts)
  end

  @spec send_request(GRPC.Client.Stream.t(), struct, keyword) :: struct
  def send_request(stream, message, opts) do
    opts = Keyword.put(opts, :send_end_stream, true)
    {:ok, stream} = send_header(stream, opts)
    send_body(stream, message, opts)
    {:ok, stream}
  end

  @spec send_header(GRPC.Client.Stream.t(), keyword) :: {:ok, GRPC.Client.Stream.t()}
  def send_header(%{channel: channel} = stream, opts) do
    headers = GRPC.Transport.HTTP2.client_headers(stream, opts)

    stream_id =
      channel
      |> h2_client_pid()
      |> :h2_connection.begin_request(headers)

    {:ok, GRPC.Client.Stream.put_payload(stream, :stream_id, stream_id)}
  end

  @spec send_body(GRPC.Client.Stream.t(), binary, send_opts) :: :ok
  def send_body(%{channel: channel, payload: %{stream_id: stream_id}}, message, opts) do
    {:ok, data, _} = GRPC.Message.to_data(message, opts)

    channel
    |> h2_client_pid()
    |> :h2_connection.send_body(stream_id, data, opts)
  end

  @spec recv_end(GRPC.Client.Stream.t(), keyword) :: any
  def recv_end(%{payload: %{stream_id: stream_id}, channel: channel}, opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        {:ok, {headers, data_list}} =
          channel
          |> h2_client_pid()
          |> :h2_client.get_response(stream_id)

        {:ok, headers, Enum.join(data_list, "")}
    after
      GRPC.Adapter.Client.timeout(opts[:deadline], opts[:timeout]) ->
        {:error, GRPC.RPCError.exception(GRPC.Status.deadline_exceeded(), "deadline exceeded")}
    end
  end

  @spec recv(GRPC.Client.Stream.t(), keyword) :: {:end_stream, any} | {:data, binary}
  def recv(%{payload: %{stream_id: stream_id}, channel: channel}, _opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        resp =
          channel
          |> h2_client_pid()
          |> :h2_client.get_response(stream_id)

        {:end_stream, resp}

      {:RECV_DATA, ^stream_id, data} ->
        {:data, data}
    end

    # TODO: timeout
  end

  defp h2_client_pid(channel) do
    pname = process_name(channel)
    pid = Process.whereis(pname)
    if !pid || !Process.alive?(pid), do: connect(channel, [])
    Process.whereis(pname)
  end
end

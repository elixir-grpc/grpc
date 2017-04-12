defmodule GRPC.Adapter.Chatterbox.Client do
  @moduledoc """
  A client(`GRPC.Channel`) adapter based on Chatterbox.

  Only one connection will be used for the same host and port. If server disconnects,
  it will try to reconnect before sending a request.
  """

  @spec connect(map, map) :: {:ok, any} | {:error, any}
  def connect(%{host: host, port: port}, payload = %{cred: nil}) do
    pname = :"grpc_chatter_client_#{host}:#{port}"
    init_args = {:client, :gen_tcp, String.to_charlist(host), port, [], :chatterbox.settings(:client)}
    do_connect(pname, init_args, payload)
  end

  def connect(%{host: host, port: port}, payload = %{cred: cred}) do
    pname = :"grpc_chatter_client_ssl_#{host}:#{port}"
    ssl_opts = [cacertfile: cred.tls.ca_path]
    init_args = {:client, :ssl, String.to_charlist(host), port, ssl_opts, :chatterbox.settings(:client)}
    do_connect(pname, init_args, payload)
  end

  defp do_connect(pname, init_args, payload) do
    case :gen_fsm.start_link({:local, pname}, :h2_connection, init_args, []) do
      {:ok, _pid} ->
        {:ok, Map.put(payload, :pname, pname)}
      {:error, {:already_started, _pid}} ->
        {:ok, Map.put(payload, :pname, pname)}
      err = {:error, _} -> err
      _ -> {:error, "Unknown error!"}
    end
  end

  @spec unary(GRPC.Client.Stream.t, struct, keyword) :: struct
  def unary(stream, message, opts) do
    {:ok, stream} = send_request(stream, message, opts)
    recv_end(stream, opts)
  end

  @spec send_request(GRPC.Client.Stream.t, struct, keyword) :: struct
  def send_request(stream, message, opts) do
    opts = Keyword.put(opts, :send_end_stream, true)
    {:ok, stream} = send_header(stream, opts)
    send_body(stream, message, opts)
    {:ok, stream}
  end

  @spec send_header(GRPC.Client.Stream.t, keyword) :: {:ok, GRPC.Client.Stream.t}
  def send_header(%{channel: channel} = stream, opts) do
    headers = GRPC.Transport.HTTP2.client_headers(stream, opts)
    pid = get_active_pname(channel)
    stream_id = :h2_connection.new_stream(pid)
    :h2_connection.send_headers(pid, stream_id, headers)
    {:ok, put_stream_id(stream, stream_id)}
  end

  @spec send_body(GRPC.Client.Stream.t, struct, keyword) :: any
  def send_body(%{channel: channel, payload: %{stream_id: stream_id}}, message, opts) do
    pid = get_active_pname(channel)
    {:ok, data} = GRPC.Message.to_data(message, opts)
    :h2_connection.send_body(pid, stream_id, data, opts)
  end

  @spec recv_end(GRPC.Client.Stream.t, keyword) :: any
  def recv_end(%{payload: %{stream_id: stream_id}, channel: channel}, opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        channel |> get_active_pname |> :h2_client.get_response(stream_id)
    after timeout(opts) ->
      # TODO: test
      raise GRPC.TimeoutError
    end
  end

  @spec recv(GRPC.Client.Stream.t, keyword) :: {:end_stream, any} | {:data, binary}
  def recv(%{payload: %{stream_id: stream_id}, channel: channel}, _opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        resp = channel |> get_active_pname |> :h2_client.get_response(stream_id)
        {:end_stream, resp}
      {:RECV_DATA, ^stream_id, data} ->
        {:data, data}
    end
    # TODO: timeout
  end

  defp put_stream_id(%{payload: payload} = stream, stream_id) do
    %{stream | payload: Map.put(payload, :stream_id, stream_id)}
  end

  defp get_active_pname(%{payload: payload = %{pname: pname}} = channel) do
    pid = Process.whereis(pname)
    if !pid || !Process.alive?(pid), do: connect(channel, payload)
    pname
  end

  defp timeout(opts) do
    cond do
      opts[:deadline] -> GRPC.TimeUtils.to_relative(opts[:deadline])
      opts[:timeout]  -> div(opts[:timeout], 1000)
      true            -> :infinity
    end
  end
end

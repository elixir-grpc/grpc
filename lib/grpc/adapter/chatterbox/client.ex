defmodule GRPC.Adapter.Chatterbox.Client do
  def connect_insecurely(%{host: host, port: port}) do
    case :h2_client.start_link(:http, String.to_charlist(host), port) do
      {:ok, pid} ->
        {:ok, %{pid: pid}}
      err = {:error, _} -> err
      _ -> {:error, "Unknown error!"}
    end
  end

  def unary(stream, message, opts) do
    {:ok, stream} = send_request(stream, message, opts)
    recv_end(stream, opts)
  end

  def send_request(stream, message, opts) do
    opts = Keyword.put(opts, :send_end_stream, true)
    {:ok, stream} = send_header(stream, opts)
    send_body(stream, message, opts)
    {:ok, stream}
  end

  def send_header(%{channel: channel} = stream, opts) do
    headers = GRPC.Transport.HTTP2.client_headers(stream, opts)
    pid = get_pid(channel)
    stream_id = :h2_connection.new_stream(pid)
    :h2_connection.send_headers(pid, stream_id, headers)
    {:ok, put_stream_id(stream, stream_id)}
  end

  def send_body(%{channel: channel, payload: %{stream_id: stream_id}}, message, opts) do
    pid = get_pid(channel)
    {:ok, data} = GRPC.Message.to_data(message, opts)
    :h2_connection.send_body(pid, stream_id, data, opts)
  end

  def recv_end(%{payload: %{stream_id: stream_id}, channel: channel}, opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        channel |> get_pid |> :h2_client.get_response(stream_id)
    after timeout(opts) ->
      # TODO: test
      raise GRPC.TimeoutError
    end
  end

  def recv(%{payload: %{stream_id: stream_id}, channel: channel}, _opts) do
    receive do
      {:END_STREAM, ^stream_id} ->
        resp = channel |> get_pid |> :h2_client.get_response(stream_id)
        {:end_stream, resp}
      {:RECV_DATA, data} ->
        {:data, data}
    end
    # TODO: timeout
  end

  defp put_stream_id(%{payload: payload} = stream, stream_id) do
    %{stream | payload: Map.put(payload, :stream_id, stream_id)}
  end

  defp get_pid(%{payload: %{pid: pid}}) do
    pid
  end

  defp timeout(opts) do
    cond do
      opts[:deadline] -> GRPC.TimeUtils.to_relative(opts[:deadline])
      opts[:timeout]  -> div(opts[:timeout], 1000)
      true            -> :infinity
    end
  end
end

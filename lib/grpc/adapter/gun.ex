defmodule GRPC.Adapter.Gun do
  @spec connect(GRPC.Channel.t(), any) :: {:ok, GRPC.Channel.t()} | {:error, any}
  def connect(channel, nil), do: connect(channel, %{})
  def connect(%{scheme: "https"} = channel, opts), do: connect_securely(channel, opts)
  def connect(channel, opts), do: connect_insecurely(channel, opts)

  defp connect_securely(%{host: host, port: port, cred: %{ssl: ssl}} = channel, opts) do
    open_opts = %{transport: :ssl, protocols: [:http2], transport_opts: ssl}
    open_opts = Map.merge(opts, open_opts)
    {:ok, conn_pid} = :gun.open(String.to_charlist(host), port, open_opts)
    case :gun.await_up(conn_pid) do
      {:ok, :http2} ->
        {:ok, Map.put(channel, :adapter_payload, %{conn_pid: conn_pid})}
      {:ok, proto} ->
        {:error, "Error when opening connection: protocol #{proto} is not http2"}
      {:error, reason} ->
        {:error, "Error when opening connection: #{inspect(reason)}"}
    end
  end

  defp connect_insecurely(%{host: host, port: port} = channel, opts) do
    open_opts = %{transport: :tcp, protocols: [:http2]}
    open_opts = Map.merge(opts, open_opts)
    {:ok, conn_pid} = :gun.open(String.to_charlist(host), port, open_opts)
    case :gun.await_up(conn_pid) do
      {:ok, :http2} ->
        {:ok, Map.put(channel, :adapter_payload, %{conn_pid: conn_pid})}
      {:ok, proto} ->
        {:error, "Error when opening connection: protocol #{proto} is not http2"}
      {:error, reason} ->
        {:error, "Error when opening connection: #{inspect(reason)}"}
    end
  end

  def unary(%{channel: %{adapter_payload: %{conn_pid: conn_pid}}} = stream, message, opts) do
    stream_ref = do_send_request(stream, message, opts)
    timeout = GRPC.Adapter.Client.timeout(opts[:deadline], opts[:timeout])
    receive_body(conn_pid, stream_ref, timeout)
  end

  @spec send_request(GRPC.Client.Stream.t(), struct, keyword) :: struct
  def send_request(stream, message, opts) do
    stream_ref = do_send_request(stream, message, opts)
    new_stream = GRPC.Client.Stream.put_payload(stream, :stream_ref, stream_ref)
    {:ok, new_stream}
  end

  defp do_send_request(%{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream, message, opts) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :gun.post(conn_pid, path, headers, data)
  end

  def send_header(%{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream, opts) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    stream_ref = :gun.post(conn_pid, path, headers)
    new_stream = GRPC.Client.Stream.put_payload(stream, :stream_ref, stream_ref)
    {:ok, new_stream}
  end

  def send_body(%{channel: channel, payload: %{stream_ref: stream_ref}} = stream, message, opts) do
    conn_pid = channel.adapter_payload[:conn_pid]
    fin = if opts[:send_end_stream], do: :fin, else: :nofin
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :gun.data(conn_pid, stream_ref, fin, data)
    {:ok, stream}
  end

  @spec recv(GRPC.Client.Stream.t(), keyword) :: {:end_stream, any} | {:data, binary}
  def recv(%{payload: %{stream_ref: stream_ref}, channel: channel}, opts) do
    timeout = GRPC.Adapter.Client.timeout(opts[:deadline], opts[:timeout])
    conn_pid = channel.adapter_payload[:conn_pid]
    receive_data(conn_pid, stream_ref, timeout)
  end

  @spec recv_end(GRPC.Client.Stream.t(), keyword) :: any
  def recv_end(%{payload: %{stream_ref: stream_ref}, channel: channel}, opts) do
    conn_pid = channel.adapter_payload[:conn_pid]
    timeout = GRPC.Adapter.Client.timeout(opts[:deadline], opts[:timeout])
    receive_body(conn_pid, stream_ref, timeout)
  end

  defp receive_data(conn_pid, stream_ref, timeout) do
    case :gun.await(conn_pid, stream_ref, timeout) do
      {:response, :fin, _status, _headers} ->
        {:error, GRPC.RPCError.exception(GRPC.Status.data_loss(), "didn't get data from peer")}
      {:response, :nofin, _status, _init_headers} ->
        receive_data(conn_pid, stream_ref, timeout)
      {:data, _is_fin, data} ->
        {:data, data}
      {:trailers, headers} ->
        {:end_stream, headers}
      {:error, :timeout} ->
        {:error, GRPC.RPCError.exception(GRPC.Status.deadline_exceeded(), "deadline exceeded")}
      {:error, {reason, msg}} ->
        {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{reason}: #{msg}")}
      {:error, reason} ->
        {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{inspect(reason)}")}
    end
  end

  defp receive_body(conn_pid, stream_ref, timeout) do
    case :gun.await(conn_pid, stream_ref, timeout) do
      {:response, :fin, _status, _headers} ->
        {:error, GRPC.RPCError.exception(GRPC.Status.data_loss(), "didn't get data from peer")}
      {:response, :nofin, _status, init_headers} ->
        case :gun.await_body(conn_pid, stream_ref, timeout) do
          {:ok, body, headers} ->
            {:ok, init_headers ++ headers, body}
          {:error, :timeout} ->
            {:error, GRPC.RPCError.exception(GRPC.Status.deadline_exceeded(), "deadline exceeded")}
          {:error, {reason, msg}} ->
            {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{reason}: #{inspect(msg)}")}
          {:error, reason} ->
            {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{inspect(reason)}")}
        end
    end
  end
end

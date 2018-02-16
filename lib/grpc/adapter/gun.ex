defmodule GRPC.Adapter.Gun do
  @spec connect(GRPC.Channel.t()) :: {:ok, GRPC.Channel.t()} | {:error, any}
  # def connect(%{scheme: "https"} = channel), do: connect_secure(channel)
  def connect(channel), do: connect_insecure(channel)

  defp connect_insecure(%{host: host, port: port} = channel) do
    {:ok, conn_pid} = :gun.open(String.to_charlist(host), port, %{protocols: [:http2]})
    case :gun.await_up(conn_pid) do
      {:ok, :http2} ->
        {:ok, Map.put(channel, :adapter_payload, %{conn_pid: conn_pid})}
      {:ok, proto} ->
        {:error, "Error when opening connection: protocol #{proto} is not http2"}
      {:error, reason} ->
        {:error, "Error when opening connection: #{inspect(reason)}"}
    end
  end

  def unary(%{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream, message, opts) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    stream_ref = :gun.post(conn_pid, path, headers, data)
    timeout = GRPC.Adapter.Client.timeout(opts[:deadline], opts[:timeout])
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
            {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{reason}: #{msg}")}
          {:error, reason} ->
            {:error, GRPC.RPCError.exception(GRPC.Status.unknown(), "#{inspect(reason)}")}
        end
    end
  end
end

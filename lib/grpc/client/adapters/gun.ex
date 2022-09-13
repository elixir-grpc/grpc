defmodule GRPC.Client.Adapters.Gun do
  @moduledoc """
  A client adapter using Gun

  `conn_pid` and `stream_ref` are stored in `GRPC.Server.Stream`.
  """

  @behaviour GRPC.Client.Adapter

  @default_transport_opts [nodelay: true]
  @max_retries 100

  @impl true
  def connect(channel, opts) when is_list(opts) do
    # handle opts as a map due to :gun.open
    opts = Map.new(opts)

    case channel do
      %{scheme: "https"} -> connect_securely(channel, opts)
      _ -> connect_insecurely(channel, opts)
    end
  end

  defp connect_securely(%{cred: %{ssl: ssl}} = channel, opts) do
    transport_opts = Map.get(opts, :transport_opts) || []

    tls_opts = Keyword.merge(@default_transport_opts ++ ssl, transport_opts)

    open_opts =
      opts
      |> Map.delete(:transport_opts)
      |> Map.merge(%{transport: :ssl, protocols: [:http2], tls_opts: tls_opts})

    do_connect(channel, open_opts)
  end

  defp connect_insecurely(channel, opts) do
    opts =
      Map.update(
        opts,
        :http2_opts,
        %{settings_timeout: :infinity},
        &Map.put(&1, :settings_timeout, :infinity)
      )

    transport_opts = Map.get(opts, :transport_opts) || []

    tcp_opts = Keyword.merge(@default_transport_opts, transport_opts)

    open_opts =
      opts
      |> Map.delete(:transport_opts)
      |> Map.merge(%{transport: :tcp, protocols: [:http2], tcp_opts: tcp_opts})

    do_connect(channel, open_opts)
  end

  defp do_connect(%{host: host, port: port} = channel, open_opts) do
    open_opts = Map.merge(%{retry: @max_retries, retry_fun: &__MODULE__.retry_fun/2}, open_opts)

    {:ok, conn_pid} = open(host, port, open_opts)

    case :gun.await_up(conn_pid) do
      {:ok, :http2} ->
        {:ok, Map.put(channel, :adapter_payload, %{conn_pid: conn_pid})}

      {:ok, proto} ->
        :gun.shutdown(conn_pid)
        {:error, "Error when opening connection: protocol #{proto} is not http2"}

      {:error, reason} ->
        :gun.shutdown(conn_pid)
        {:error, reason}
    end
  end

  @impl true
  def disconnect(%{adapter_payload: %{conn_pid: gun_pid}} = channel)
      when is_pid(gun_pid) do
    :ok = :gun.shutdown(gun_pid)
    {:ok, %{channel | adapter_payload: %{conn_pid: nil}}}
  end

  def disconnect(%{adapter_payload: %{conn_pid: nil}} = channel) do
    {:ok, channel}
  end

  defp open({:local, socket_path}, _port, open_opts),
    do: :gun.open_unix(socket_path, open_opts)

  defp open(host, port, open_opts),
    do: :gun.open(String.to_charlist(host), port, open_opts)

  @impl true
  def send_request(stream, message, opts) do
    stream_ref = do_send_request(stream, message, opts)
    GRPC.Client.Stream.put_payload(stream, :stream_ref, stream_ref)
  end

  defp do_send_request(
         %{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream,
         message,
         opts
       ) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :gun.post(conn_pid, path, headers, data)
  end

  def send_headers(
        %{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream,
        opts
      ) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    stream_ref = :gun.post(conn_pid, path, headers)
    GRPC.Client.Stream.put_payload(stream, :stream_ref, stream_ref)
  end

  def send_data(%{channel: channel, payload: %{stream_ref: stream_ref}} = stream, message, opts) do
    conn_pid = channel.adapter_payload[:conn_pid]
    fin = if opts[:send_end_stream], do: :fin, else: :nofin
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :gun.data(conn_pid, stream_ref, fin, data)
    stream
  end

  def end_stream(%{channel: channel, payload: %{stream_ref: stream_ref}} = stream) do
    conn_pid = channel.adapter_payload[:conn_pid]
    :gun.data(conn_pid, stream_ref, :fin, "")
    stream
  end

  def cancel(%{conn_pid: conn_pid}, %{stream_ref: stream_ref}) do
    :gun.cancel(conn_pid, stream_ref)
  end

  @impl true
  def recv_headers(%{conn_pid: conn_pid}, %{stream_ref: stream_ref}, opts) do
    case await(conn_pid, stream_ref, opts[:timeout]) do
      {:response, headers, fin} ->
        {:ok, headers, fin}

      {:error, _} = error ->
        error

      other ->
        {:error,
         GRPC.RPCError.exception(
           GRPC.Status.unknown(),
           "unexpected when waiting for headers: #{inspect(other)}"
         )}
    end
  end

  @impl true
  def recv_data_or_trailers(%{conn_pid: conn_pid}, %{stream_ref: stream_ref}, opts) do
    case await(conn_pid, stream_ref, opts[:timeout]) do
      data = {:data, _} ->
        data

      trailers = {:trailers, _} ->
        trailers

      {:error, _} = error ->
        error

      other ->
        {:error,
         GRPC.RPCError.exception(
           GRPC.Status.unknown(),
           "unexpected when waiting for data: #{inspect(other)}"
         )}
    end
  end

  defp await(conn_pid, stream_ref, timeout) do
    # We should use server timeout for most time
    timeout =
      if is_integer(timeout) do
        timeout * 2
      else
        timeout
      end

    case :gun.await(conn_pid, stream_ref, timeout) do
      {:response, :fin, status, headers} ->
        if status == 200 do
          headers = Enum.into(headers, %{})

          case headers["grpc-status"] do
            nil ->
              {:error,
               GRPC.RPCError.exception(
                 GRPC.Status.internal(),
                 "shouldn't finish when getting headers"
               )}

            "0" ->
              {:response, headers, :fin}

            _ ->
              {:error,
               GRPC.RPCError.exception(
                 String.to_integer(headers["grpc-status"]),
                 headers["grpc-message"]
               )}
          end
        else
          {:error,
           GRPC.RPCError.exception(
             GRPC.Status.internal(),
             "status got is #{status} instead of 200"
           )}
        end

      {:response, :nofin, status, headers} ->
        if status == 200 do
          headers = Enum.into(headers, %{})

          if headers["grpc-status"] && headers["grpc-status"] != "0" do
            {:error,
             GRPC.RPCError.exception(
               String.to_integer(headers["grpc-status"]),
               headers["grpc-message"]
             )}
          else
            {:response, headers, :nofin}
          end
        else
          {:error,
           GRPC.RPCError.exception(
             GRPC.Status.internal(),
             "status got is #{status} instead of 200"
           )}
        end

      {:data, :fin, data} ->
        {:data, data}

      {:data, :nofin, data} ->
        {:data, data}

      trailers = {:trailers, _} ->
        trailers

      {:error, :timeout} ->
        {:error,
         GRPC.RPCError.exception(
           GRPC.Status.deadline_exceeded(),
           "timeout when waiting for server"
         )}

      {:error, {reason, msg}} when reason in [:stream_error, :connection_error] ->
        {:error,
         GRPC.RPCError.exception(GRPC.Status.internal(), "#{inspect(reason)}: #{inspect(msg)}")}

      {:error, {reason, msg}} ->
        {:error,
         GRPC.RPCError.exception(GRPC.Status.unknown(), "#{inspect(reason)}: #{inspect(msg)}")}

      other ->
        {:error,
         GRPC.RPCError.exception(
           GRPC.Status.unknown(),
           "unexpected message when waiting for server: #{inspect(other)}"
         )}
    end
  end

  def retry_fun(retries, _opts) do
    curr = @max_retries - retries + 1

    timeout =
      if curr < 11 do
        :math.pow(1.6, curr - 1) * 1000
      else
        120_000
      end

    jitter =
      if function_exported?(:rand, :uniform_real, 0) do
        (:rand.uniform_real() - 0.5) / 2.5
      else
        (:rand.uniform() - 0.5) / 2.5
      end

    timeout = round(timeout + jitter * timeout)
    %{retries: retries - 1, timeout: timeout}
  end
end

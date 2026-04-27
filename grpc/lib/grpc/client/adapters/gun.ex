defmodule GRPC.Client.Adapters.Gun do
  @moduledoc """
  A client adapter using Gun

  `conn_pid` stores the adapter-managed connection process pid and per-request
  `stream_ref` values are stored in `GRPC.Client.Stream`.
  """

  @behaviour GRPC.Client.Adapter

  alias GRPC.Client.Adapters.Gun.ConnectionProcess

  @default_tcp_opts [nodelay: true]
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

    tls_opts = Keyword.merge(ssl, transport_opts)

    open_opts =
      opts
      |> Map.delete(:transport_opts)
      |> Map.merge(%{
        transport: :ssl,
        protocols: [:http2],
        tcp_opts: @default_tcp_opts,
        tls_opts: tls_opts
      })

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

    tcp_opts = Keyword.merge(@default_tcp_opts, transport_opts)

    open_opts =
      opts
      |> Map.delete(:transport_opts)
      |> Map.merge(%{transport: :tcp, protocols: [:http2], tcp_opts: tcp_opts})

    do_connect(channel, open_opts)
  end

  defp do_connect(channel, open_opts) do
    open_opts = Map.merge(%{retry: @max_retries, retry_fun: &__MODULE__.retry_fun/2}, open_opts)

    case ConnectionProcess.connect(channel, open_opts) do
      {:ok, adapter_payload} ->
        {:ok, Map.put(channel, :adapter_payload, adapter_payload)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def disconnect(%{adapter_payload: %{conn_pid: conn_pid}} = channel)
      when is_pid(conn_pid) do
    :ok = ConnectionProcess.disconnect(conn_pid)
    {:ok, %{channel | adapter_payload: %{conn_pid: nil}}}
  end

  def disconnect(%{adapter_payload: %{conn_pid: nil}} = channel) do
    {:ok, channel}
  end

  @impl true
  def send_request(stream, message, opts) do
    {stream_ref, response_pid} = do_send_request(stream, message, opts)

    stream
    |> GRPC.Client.Stream.put_payload(:stream_ref, stream_ref)
    |> GRPC.Client.Stream.put_payload(:response_pid, response_pid)
  end

  defp do_send_request(
         %{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream,
         message,
         opts
       ) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)

    {:ok, %{stream_ref: stream_ref, response_pid: response_pid}} =
      ConnectionProcess.request(conn_pid, path, headers, data)

    {stream_ref, response_pid}
  end

  @impl true
  def send_headers(
        %{channel: %{adapter_payload: %{conn_pid: conn_pid}}, path: path} = stream,
        opts
      ) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)

    {:ok, %{stream_ref: stream_ref, response_pid: response_pid}} =
      ConnectionProcess.open_stream(conn_pid, path, headers)

    stream
    |> GRPC.Client.Stream.put_payload(:stream_ref, stream_ref)
    |> GRPC.Client.Stream.put_payload(:response_pid, response_pid)
  end

  @impl true
  def send_data(%{channel: channel, payload: %{stream_ref: stream_ref}} = stream, message, opts) do
    conn_pid = channel.adapter_payload[:conn_pid]
    fin = if opts[:send_end_stream], do: :fin, else: :nofin
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :ok = ConnectionProcess.send_data(conn_pid, stream_ref, fin, data)
    stream
  end

  @impl true
  def end_stream(%{channel: channel, payload: %{stream_ref: stream_ref}} = stream) do
    conn_pid = channel.adapter_payload[:conn_pid]
    :ok = ConnectionProcess.send_data(conn_pid, stream_ref, :fin, "")
    stream
  end

  @impl true
  def cancel(stream) do
    %{
      channel: %{adapter_payload: %{conn_pid: conn_pid}},
      payload: %{stream_ref: stream_ref}
    } = stream

    ConnectionProcess.cancel(conn_pid, stream_ref)
  end

  @impl true
  def receive_data(
        %{server_stream: true} = stream,
        opts
      ) do
    %{payload: payload} = stream

    case recv_headers(payload, opts) do
      {:ok, headers, :fin} ->
        handle_fin_response(headers, opts)

      {:ok, headers, :nofin} ->
        handle_streaming_nofin_response(stream, headers, opts)

      {:error, _} = error ->
        error
    end
  end

  def receive_data(stream, opts) do
    %{payload: payload} = stream

    case recv_headers(payload, opts) do
      {:ok, headers, :fin} ->
        handle_fin_response(headers, opts)

      {:ok, headers, :nofin} ->
        handle_nofin_response(payload, stream, headers, opts)

      {:error, _} = error ->
        error
    end
  end

  defp handle_fin_response(headers, opts) do
    # Trailers-only response: headers contain trailers, check status
    with :ok <- parse_trailers(headers) do
      if opts[:return_headers] do
        {:ok, [], %{headers: headers}}
      else
        {:ok, []}
      end
    end
  end

  defp handle_streaming_nofin_response(stream, headers, opts) do
    response = response_stream(:nofin, stream, opts)

    if opts[:return_headers] do
      {:ok, response, %{headers: headers}}
    else
      {:ok, response}
    end
  end

  defp handle_nofin_response(payload, stream, headers, opts) do
    # Regular response: fetch body and trailers
    with {:ok, body, trailers} <- recv_body(payload, opts),
         {:ok, response, embedded_trailers} <- parse_response(stream, headers, body, trailers) do
      if opts[:return_headers] do
        all_trailers = Map.merge(trailers, embedded_trailers)

        {
          :ok,
          response,
          %{headers: headers, trailers: all_trailers}
        }
      else
        {:ok, response}
      end
    end
  end

  defp recv_headers(%{response_pid: response_pid}, opts) do
    case await(response_pid, opts[:timeout]) do
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

  defp recv_data_or_trailers(%{response_pid: response_pid}, opts) do
    case await(response_pid, opts[:timeout]) do
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

  defp await(response_pid, timeout) do
    # We should use server timeout for most time
    timeout =
      if is_integer(timeout) do
        timeout * 2
      else
        timeout
      end

    case GRPC.Client.Adapters.Gun.StreamResponseProcess.await(response_pid, timeout) do
      {:response, :fin, status, headers} ->
        if status == 200 do
          headers = GRPC.Transport.HTTP2.decode_headers(headers)
          {:response, headers, :fin}
        else
          {:error,
           GRPC.RPCError.exception(
             GRPC.Status.internal(),
             "status got is #{status} instead of 200"
           )}
        end

      {:response, :nofin, status, headers} ->
        if status == 200 do
          headers = GRPC.Transport.HTTP2.decode_headers(headers)
          {:response, headers, :nofin}
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

  defp recv_body(stream_payload, opts) do
    recv_body(stream_payload, "", opts)
  end

  defp recv_body(stream_payload, acc, opts) do
    case recv_data_or_trailers(stream_payload, opts) do
      {:data, data} ->
        recv_body(stream_payload, <<acc::binary, data::binary>>, opts)

      {:trailers, trailers} ->
        {:ok, acc, GRPC.Transport.HTTP2.decode_headers(trailers)}

      err ->
        err
    end
  end

  defp response_stream(
         :nofin,
         %{
           response_mod: res_mod,
           codec: codec,
           payload: payload
         },
         opts
       ) do
    state = %{
      payload: payload,
      buffer: <<>>,
      fin: false,
      need_more: true,
      opts: opts,
      response_mod: res_mod,
      codec: codec
    }

    Stream.unfold(state, fn s -> read_stream(s) end)
  end

  defp read_stream(%{buffer: <<>>, fin: true, fin_resp: nil}), do: nil

  defp read_stream(%{buffer: <<>>, fin: true, fin_resp: fin_resp} = s),
    do: {fin_resp, Map.put(s, :fin_resp, nil)}

  defp read_stream(
         %{
           payload: payload,
           buffer: buffer,
           need_more: true,
           opts: opts
         } = stream
       ) do
    case recv_data_or_trailers(payload, opts) do
      {:data, data} ->
        stream
        |> Map.put(:need_more, false)
        |> Map.put(:buffer, buffer <> data)
        |> read_stream()

      {:trailers, trailers} ->
        update_stream_with_trailers(stream, trailers, opts[:return_headers])

      error = {:error, _} ->
        {error, %{buffer: <<>>, fin: true, fin_resp: nil}}
    end
  end

  defp read_stream(
         %{buffer: buffer, need_more: false, response_mod: res_mod, codec: codec, opts: opts} =
           stream
       ) do
    case GRPC.Message.get_message(buffer) do
      {{:trailers, trailers}, rest} ->
        new_stream =
          stream
          |> update_stream_with_trailers(trailers, opts[:return_headers])
          |> Map.put(:buffer, rest)

        {{:ok, trailers}, new_stream}

      {{_, message}, rest} ->
        reply = codec.decode(message, res_mod)
        new_stream = Map.put(stream, :buffer, rest)
        {{:ok, reply}, new_stream}

      _ ->
        read_stream(Map.put(stream, :need_more, true))
    end
  end

  defp parse_response(
         %{response_mod: res_mod, codec: codec, accepted_compressors: accepted_compressors},
         headers,
         body,
         trailers
       ) do
    with :ok <- parse_trailers(trailers),
         compressor <- get_compressor(headers, accepted_compressors),
         body <- get_body(codec, body),
         {:ok, msg, remaining} <- GRPC.Message.from_data(%{compressor: compressor}, body) do
      {:ok, codec.decode(msg, res_mod), check_for_trailers(remaining, compressor)}
    end
  end

  defp check_for_trailers(<<>>, _compressor), do: %{}

  defp check_for_trailers(body, compressor) do
    case GRPC.Message.from_data(%{compressor: compressor}, body) do
      {:trailers, trailers, <<>>} -> trailers
      _ -> %{}
    end
  end

  defp update_stream_with_trailers(stream, trailers, return_headers?) do
    trailers = GRPC.Transport.HTTP2.decode_headers(trailers)

    case parse_trailers(trailers) do
      :ok ->
        fin_resp = if return_headers?, do: {:trailers, trailers}

        stream
        |> Map.put(:fin, true)
        |> Map.put(:fin_resp, fin_resp)
        |> read_stream()

      error ->
        {error, %{buffer: <<>>, fin: true, fin_resp: nil}}
    end
  end

  defp parse_trailers(trailers) do
    status = String.to_integer(trailers["grpc-status"])

    if status == GRPC.Status.ok() do
      :ok
    else
      rpc_error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: trailers["grpc-message"],
          encoded_details_bin: trailers["grpc-status-details-bin"]
        })

      {:error, rpc_error}
    end
  end

  defp get_compressor(%{"grpc-encoding" => encoding} = _headers, accepted_compressors) do
    Enum.find(accepted_compressors, nil, fn c -> c.name() == encoding end)
  end

  defp get_compressor(_headers, _accepted_compressors), do: nil

  defp get_body(codec, body) do
    if function_exported?(codec, :unpack_from_channel, 1) do
      codec.unpack_from_channel(body)
    else
      body
    end
  end
end

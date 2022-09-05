defmodule GRPC.Client.Adapters.Mint do
  @moduledoc """
  A client adapter using mint
  """

  alias GRPC.{Channel, Credential}
  alias __MODULE__.ConnectionProcess

  @behaviour GRPC.Client.Adapter

  @default_connect_opts [protocols: [:http2]]
  @default_transport_opts [timeout: :infinity]

  @impl true
  def connect(%{host: host, port: port} = channel, opts \\ []) do
    opts = Keyword.merge(@default_connect_opts, connect_opts(channel, opts))

    channel
    |> mint_scheme()
    |> ConnectionProcess.start_link(host, port, opts)
    |> case do
      {:ok, pid} -> {:ok, %{channel | adapter_payload: %{conn_pid: pid}}}
      # TODO add proper error handling
      _error -> {:ok, %{channel | adapter_payload: %{conn_pid: nil}}}
    end
  end

  @impl true
  def disconnect(%{adapter_payload: %{conn_pid: pid}} = channel)
      when is_pid(pid) do
    ConnectionProcess.disconnect(pid)
    {:ok, %{channel | adapter_payload: %{conn_pid: nil}}}
  end

  def disconnect(%{adapter_payload: %{conn_pid: nil}} = channel) do
    {:ok, channel}
  end

  @impl true
  def send_request(
        %{channel: %{adapter_payload: %{conn_pid: pid}}, path: path, server_stream: stream?} =
          stream,
        message,
        opts
      )
      when is_pid(pid) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    {:ok, response} = ConnectionProcess.request(pid, "POST", path, headers, data, stream?)
    GRPC.Client.Stream.put_payload(stream, :response, response)
  end

  @impl true
  def receive_data(%{server_stream: true, payload: %{response: response}} = stream, opts) do
    with {%{headers: headers}, request_ref} <- response,
         stream_response <- build_response_stream(stream, request_ref) do
      if(opts[:return_headers]) do
        {:ok, stream_response, %{headers: headers}}
      else
        {:ok, stream_response}
      end
    end
  end

  def receive_data(
        %{
          response_mod: res_mod,
          codec: codec,
          accepted_compressors: accepted_compressors,
          payload: %{response: response}
        } = _stream,
        _opts
      ) do
    with %{data: body, headers: headers} <- response,
         compressor <- get_compressor(headers, accepted_compressors),
         body <- get_body(codec, body),
         {:ok, msg} <- GRPC.Message.from_data(%{compressor: compressor}, body) do
      {:ok, codec.decode(msg, res_mod)}
    end
  end

  defp connect_opts(%Channel{scheme: "https"} = channel, opts) do
    %Credential{ssl: ssl} = Map.get(channel, :cred, %Credential{})

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.merge(ssl)

    [transport_opts: Keyword.merge(@default_transport_opts, transport_opts)]
  end

  defp connect_opts(_channel, opts) do
    transport_opts = Keyword.get(opts, :transport_opts, [])
    [transport_opts: Keyword.merge(@default_transport_opts, transport_opts)]
  end

  defp mint_scheme(%Channel{scheme: "https"} = _channel), do: :https
  defp mint_scheme(_channel), do: :http

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

  defp build_response_stream(grpc_stream, request_ref) do
    state = %{
      buffer: <<>>,
      done: false,
      request_ref: request_ref,
      grpc_stream: grpc_stream
    }

    Stream.unfold(state, fn s -> process_stream(s) end)
  end

  defp process_stream(%{buffer: <<>>, done: true}) do
    nil
  end

  defp process_stream(%{done: true} = state) do
    parse_message(state, "", true)
  end

  defp process_stream(
         %{request_ref: ref, grpc_stream: %{channel: %{adapter_payload: %{conn_pid: pid}}}} =
           state
       ) do
    case ConnectionProcess.process_stream_data(pid, ref) do
      {nil, false = _done?} ->
        Process.sleep(500)
        process_stream(state)

      {nil = _data, true = _done?} ->
        parse_message(state, "", true)

      {data, done?} ->
        parse_message(state, data, done?)
    end
  end

  defp parse_message(
         %{buffer: buffer, grpc_stream: %{response_mod: res_mod, codec: codec}} = state,
         data,
         done?
       ) do
    case GRPC.Message.get_message(buffer <> data) do
      {{_, message}, rest} ->
        reply = codec.decode(message, res_mod)
        new_state = %{state | buffer: rest, done: done?}
        {{:ok, reply}, new_state}

      _ ->
        state
        |> Map.put(:buffer, buffer <> data)
        |> Map.put(:done, done?)
        |> process_stream()
    end
  end
end

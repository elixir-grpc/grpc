defmodule GRPC.Client.Adapters.Mint do
  @moduledoc """
  A client adapter using mint
  """

  alias GRPC.{Channel, Credential}
  alias GRPC.Client.Adapters.Mint.{ConnectionProcess, StreamResponseProcess}

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
        %{channel: %{adapter_payload: %{conn_pid: pid}}, path: path, server_stream: false} =
          stream,
        message,
        opts
      )
      when is_pid(pid) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    response = ConnectionProcess.request(pid, "POST", path, headers, data)
    GRPC.Client.Stream.put_payload(stream, :response, response)
  end

  def send_request(
        %{channel: %{adapter_payload: %{conn_pid: pid}}, path: path, server_stream: true} =
          stream,
        message,
        opts
      )
      when is_pid(pid) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    {:ok, stream_response_pid} = StreamResponseProcess.start_link(stream, opts[:return_headers] || false)

    response =
      ConnectionProcess.request(pid, "POST", path, headers, data,
        streamed_response: true,
        stream_response_pid: stream_response_pid
      )

    stream
    |> GRPC.Client.Stream.put_payload(:response, response)
    |> GRPC.Client.Stream.put_payload(:stream_response_pid, stream_response_pid)
  end

  @impl true
  def receive_data(
        %{server_stream: true, payload: %{response: response, stream_response_pid: pid}},
        opts
      ) do
    with {:ok, headers} <- response do
      stream = StreamResponseProcess.build_stream(pid)
      case opts[:return_headers] do
        true -> {:ok, stream, headers}
        _any -> {:ok, stream}
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
        opts
      ) do
    with {:ok, %{data: body, headers: headers}} <- response,
         compressor <- get_compressor(headers, accepted_compressors),
         body <- get_body(codec, body),
         {:ok, msg} <- GRPC.Message.from_data(%{compressor: compressor}, body) do
      if opts[:return_headers] do
        {:ok, codec.decode(msg, res_mod), %{headers: headers}}
      else
        {:ok, codec.decode(msg, res_mod)}
      end
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
end

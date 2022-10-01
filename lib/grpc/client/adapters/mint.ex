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
      error -> raise "An error happened while trying to opening the connection: #{inspect(error)}"
    end
  end

  @impl true
  def disconnect(%{adapter_payload: %{conn_pid: pid}} = channel)
      when is_pid(pid) do
    :ok = ConnectionProcess.disconnect(pid)
    {:ok, %{channel | adapter_payload: nil}}
  end

  def disconnect(%{adapter_payload: nil} = channel) do
    {:ok, channel}
  end

  @impl true
  def send_request(%{channel: %{adapter_payload: nil}}, _message, _opts),
    do: raise("Can't perform a request without a connection process")

  def send_request(
        %{channel: %{adapter_payload: %{conn_pid: pid}}, path: path} = stream,
        message,
        opts
      )
      when is_pid(pid) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)

    {:ok, stream_response_pid} =
      StreamResponseProcess.start_link(stream, opts[:return_headers] || false)

    response =
      ConnectionProcess.request(pid, "POST", path, headers, data,
        stream_response_pid: stream_response_pid
      )

    stream
    |> GRPC.Client.Stream.put_payload(:response, response)
    |> GRPC.Client.Stream.put_payload(:stream_response_pid, stream_response_pid)
  end

  @impl true
  def receive_data(stream, opts) do
    cond do
      bidirectional_stream?(stream) ->
        do_receive_data(stream, :bidirectional_stream, opts)

      unary_request_stream_response?(stream) ->
        do_receive_data(stream, :unary_request_stream_response, opts)

      stream_request_unary_response?(stream) ->
        do_receive_data(stream, :stream_request_unary_response, opts)

      unary_request_response?(stream) ->
        do_receive_data(stream, :unary_request_response, opts)

      true ->
        handle_errors_receive_data(stream, opts)
    end
  end

  @impl true
  def send_headers(%{channel: %{adapter_payload: nil}}, _opts),
    do: raise("Can't start a client stream without a connection process")

  def send_headers(%{channel: %{adapter_payload: %{conn_pid: pid}}, path: path} = stream, opts) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)

    {:ok, stream_response_pid} =
      StreamResponseProcess.start_link(stream, opts[:return_headers] || false)

    response =
      ConnectionProcess.request(pid, "POST", path, headers, :stream,
        stream_response_pid: stream_response_pid
      )

    stream
    |> GRPC.Client.Stream.put_payload(:response, response)
    |> GRPC.Client.Stream.put_payload(:stream_response_pid, stream_response_pid)
  end

  @impl true
  def send_data(
        %{
          channel: %{adapter_payload: %{conn_pid: pid}},
          payload: %{response: {:ok, %{request_ref: request_ref}}}
        } = stream,
        message,
        opts
      ) do
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    :ok = ConnectionProcess.stream_request_body(pid, request_ref, data)
    # TODO: check for trailer headers to be sent here
    if opts[:send_end_stream], do: ConnectionProcess.stream_request_body(pid, request_ref, :eof)
    stream
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

  def check_for_error(responses) do
    error = Keyword.get(responses, :error)

    if error, do: error, else: :ok
  end

  defp append_trailers(headers, responses) do
    case Keyword.get(responses, :trailers) do
      nil -> %{headers: headers}
      trailers -> %{headers: headers, trailers: trailers}
    end
  end

  defp do_receive_data(%{payload: %{stream_response_pid: pid}}, :bidirectional_stream, _opts) do
    stream = StreamResponseProcess.build_stream(pid)
    {:ok, stream}
  end

  defp do_receive_data(
         %{payload: %{response: {:ok, headers}, stream_response_pid: pid}},
         :unary_request_stream_response,
         opts
       ) do
    stream = StreamResponseProcess.build_stream(pid)

    if opts[:return_headers] do
      {:ok, stream, headers}
    else
      {:ok, stream}
    end
  end

  defp do_receive_data(
         %{payload: %{response: {:ok, %{request_ref: _ref}}, stream_response_pid: pid}},
         :stream_request_unary_response,
         opts
       ) do
    with stream <- StreamResponseProcess.build_stream(pid),
         responses <- Enum.to_list(stream),
         :ok <- check_for_error(responses) do
      data = Keyword.fetch!(responses, :ok)

      if opts[:return_headers] do
        {:ok, data, append_trailers(Keyword.get(responses, :headers), responses)}
      else
        {:ok, data}
      end
    end
  end

  defp do_receive_data(
         %{payload: %{response: {:ok, %{headers: headers}}, stream_response_pid: pid}},
         :unary_request_response,
         opts
       ) do
    responses = Enum.to_list(StreamResponseProcess.build_stream(pid))

    with :ok <- check_for_error(responses) do
      data = Keyword.fetch!(responses, :ok)

      if(opts[:return_headers]) do
        {:ok, data, append_trailers(headers, responses)}
      else
        {:ok, data}
      end
    end
  end

  def handle_errors_receive_data(_stream, _opts) do
    raise "TODO: Implement"
  end

  defp bidirectional_stream?(%GRPC.Client.Stream{
         server_stream: true,
         payload: %{response: {:ok, %{request_ref: ref}}, stream_response_pid: pid}
       })
       when is_reference(ref) and is_pid(pid),
       do: true

  defp bidirectional_stream?(_stream), do: false

  defp unary_request_stream_response?(%GRPC.Client.Stream{
         server_stream: true,
         payload: %{response: {:ok, _headers}, stream_response_pid: pid}
       })
       when is_pid(pid),
       do: true

  defp unary_request_stream_response?(_stream), do: false

  defp stream_request_unary_response?(%GRPC.Client.Stream{
         server_stream: false,
         payload: %{response: {:ok, %{request_ref: ref}}, stream_response_pid: pid}
       })
       when is_pid(pid) and is_reference(ref),
       do: true

  defp stream_request_unary_response?(_stream), do: false

  defp unary_request_response?(%GRPC.Client.Stream{
         server_stream: false,
         payload: %{response: {:ok, _headers}, stream_response_pid: pid}
       })
       when is_pid(pid),
       do: true

  defp unary_request_response?(_stream), do: false
end

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

  def receive_data(%{payload: %{response: response, stream_response_pid: pid}}, opts) do
    with {:ok, %{headers: headers}} <- response,
         stream <- StreamResponseProcess.build_stream(pid),
         responses <- Enum.into(stream, []),
         :ok <- check_for_error(responses) do
      {:ok, data} = Enum.find(responses, fn {status, _data} -> status == :ok end)

      case opts[:return_headers] do
        true -> {:ok, data, append_trailers(headers, responses)}
        _any -> {:ok, data}
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

  def check_for_error(responses) do
    error = Enum.find(responses, fn {status, _data} -> status == :error end)
    if error == nil, do: :ok, else: error
  end

  defp append_trailers(headers, responses) do
    responses
    |> Enum.find(fn {status, _data} -> status == :trailers end)
    |> case do
      nil -> %{headers: headers}
      {:trailers, trailers} -> %{headers: headers, trailers: trailers}
    end
  end
end

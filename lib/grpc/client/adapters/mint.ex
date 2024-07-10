defmodule GRPC.Client.Adapters.Mint do
  @moduledoc """
  A client adapter using Mint.
  """

  alias GRPC.Channel
  alias GRPC.Client.Adapters.Mint.ConnectionProcess
  alias GRPC.Client.Adapters.Mint.StreamResponseProcess
  alias GRPC.Credential

  @behaviour GRPC.Client.Adapter

  @default_connect_opts [
    protocols: [:http2]
  ]
  @default_client_settings [
    initial_window_size: 8_000_000,
    max_frame_size: 8_000_000
  ]
  @default_transport_opts [timeout: :infinity]

  @doc """
  Connects using Mint based on the provided configs. Options
    * `:transport_opts`: Defaults to `[timeout: :infinity]`, given the nature of H2 connections (with support to
      long-lived streams) this default is set to avoid timeouts while waiting for server streams to complete. The other
      options may vary based on the transport used for this connection (tcp or ssl). Check [Mint.HTTP.connect/4](https://hexdocs.pm/mint/Mint.HTTP.html#connect/4)
    * `:client_settings`: Defaults to `[initial_window_size: 8_000_000, max_frame_size: 8_000_000]`, a larger default
      window size ensures that the number of packages exchanges is smaller, thus speeding up the requests by reducing the
      amount of networks round trip, with the cost of having larger packages reaching the server per connection.
      Check [Mint.HTTP2.setting() type](https://hexdocs.pm/mint/Mint.HTTP2.html#t:setting/0) for additional configs.
  """
  @impl true
  def connect(%{host: host, port: port} = channel, opts \\ []) do
    # Added :config_options to facilitate testing.
    {config_opts, opts} = Keyword.pop(opts, :config_options, [])
    module_opts = Application.get_env(:grpc, __MODULE__, config_opts)

    opts = connect_opts(channel, opts) |> merge_opts(module_opts)

    Process.flag(:trap_exit, true)

    channel
    |> mint_scheme()
    |> ConnectionProcess.start_link(host, port, opts)
    |> case do
      {:ok, pid} ->
        {:ok, %{channel | adapter_payload: %{conn_pid: pid}}}

      error ->
        {:error, "Error while opening connection: #{inspect(error)}"}
    end
  catch
    :exit, reason ->
      {:error, "Error while opening connection: #{inspect(reason)}"}
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
    do: raise(ArgumentError, "Can't perform a request without a connection process")

  def send_request(stream, message, opts) do
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    do_request(stream, opts, data)
  end

  @impl true
  def receive_data(stream, opts) do
    if success_response?(stream) do
      do_receive_data(stream, stream.grpc_type, opts)
    else
      handle_errors_receive_data(stream, opts)
    end
  end

  @impl true
  def send_headers(%{channel: %{adapter_payload: nil}}, _opts),
    do: raise("Can't start a client stream without a connection process")

  def send_headers(stream, opts) do
    do_request(stream, opts, :stream)
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
    if opts[:send_end_stream], do: ConnectionProcess.stream_request_body(pid, request_ref, :eof)
    stream
  end

  @impl true
  def end_stream(
        %{
          channel: %{adapter_payload: %{conn_pid: pid}},
          payload: %{response: {:ok, %{request_ref: request_ref}}}
        } = stream
      ) do
    ConnectionProcess.stream_request_body(pid, request_ref, :eof)
    stream
  end

  @impl true
  def cancel(stream) do
    %{
      channel: %{adapter_payload: %{conn_pid: conn_pid}},
      payload: %{response: {:ok, %{request_ref: request_ref}}}
    } = stream

    ConnectionProcess.cancel(conn_pid, request_ref)
  end

  defp connect_opts(%Channel{scheme: "https"} = channel, opts) do
    %Credential{ssl: ssl} = Map.get(channel, :cred) || %Credential{}

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.merge(ssl)

    client_settings = Keyword.get(opts, :client_settings, @default_client_settings)

    [
      transport_opts: Keyword.merge(@default_transport_opts, transport_opts),
      client_settings: client_settings
    ]
  end

  defp connect_opts(_channel, opts) do
    transport_opts = Keyword.get(opts, :transport_opts, [])
    client_settings = Keyword.get(opts, :client_settings, @default_client_settings)

    [
      transport_opts: Keyword.merge(@default_transport_opts, transport_opts),
      client_settings: client_settings
    ]
  end

  defp merge_opts(opts, module_opts) do
    opts = Keyword.merge(opts, module_opts)
    Keyword.merge(@default_connect_opts, opts)
  end

  defp mint_scheme(%Channel{scheme: "https"} = _channel), do: :https
  defp mint_scheme(_channel), do: :http

  defp do_receive_data(%{payload: %{stream_response_pid: pid}}, request_type, opts)
       when request_type in [:bidirectional_stream, :server_stream] do
    produce_trailers? = opts[:return_headers] == true
    stream = StreamResponseProcess.build_stream(pid, produce_trailers?)
    headers_or_error = Enum.at(stream, 0)
    # if this check fails then an error tuple will be returned
    with {:headers, headers} <- headers_or_error do
      if opts[:return_headers] do
        {:ok, stream, %{headers: headers}}
      else
        {:ok, stream}
      end
    end
  end

  defp do_receive_data(
         %{payload: %{stream_response_pid: pid}},
         request_type,
         opts
       )
       when request_type in [:client_stream, :unary] do
    responses = pid |> StreamResponseProcess.build_stream() |> Enum.to_list()

    with :ok <- check_for_error(responses) do
      data = Keyword.fetch!(responses, :ok)

      if opts[:return_headers] do
        {:ok, data, get_headers_and_trailers(responses)}
      else
        {:ok, data}
      end
    end
  end

  def handle_errors_receive_data(%GRPC.Client.Stream{payload: %{response: response}}, _opts) do
    {:error, "Error occurred while receiving data: #{inspect(response)}"}
  end

  defp success_response?(%GRPC.Client.Stream{
         payload: %{response: {:ok, _resp}}
       }),
       do: true

  defp success_response?(_stream), do: false

  defp do_request(
         %{channel: %{adapter_payload: %{conn_pid: pid}}, path: path} = stream,
         opts,
         body
       ) do
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)

    {:ok, stream_response_pid} =
      StreamResponseProcess.start_link(stream, return_headers_for_request?(stream, opts))

    response =
      ConnectionProcess.request(pid, "POST", path, headers, body,
        stream_response_pid: stream_response_pid
      )

    stream
    |> GRPC.Client.Stream.put_payload(:response, response)
    |> GRPC.Client.Stream.put_payload(:stream_response_pid, stream_response_pid)
  end

  defp get_headers_and_trailers(responses) do
    %{headers: Keyword.get(responses, :headers), trailers: Keyword.get(responses, :trailers)}
  end

  def check_for_error(responses) do
    error = Keyword.get(responses, :error)

    if error, do: {:error, error}, else: :ok
  end

  defp return_headers_for_request?(%GRPC.Client.Stream{grpc_type: type}, _opts)
       when type in [:bidirectional_stream, :server_stream] do
    true
  end

  defp return_headers_for_request?(_stream, opts) do
    # Explicitly check for true to ensure the boolean type here
    opts[:return_headers] == true
  end
end

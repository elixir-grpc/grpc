defmodule GRPC.Client.Adapters.Finch do
  @moduledoc """
  A client adapter using Finch.
  """

  alias GRPC.Channel
  alias GRPC.Credential
  alias Grpc.Client.Adapters.Finch.StreamRequestProcess
  alias Grpc.Client.Adapters.Finch.CustomStream

  @behaviour GRPC.Client.Adapter

  @finch_instance_name Finch.GRPC
  @default_conn_opts [
    client_settings: [
      initial_window_size: 8_000_000,
      max_frame_size: 8_000_000
    ]
  ]

  @doc """
  Connects using Finch (Mint backend) based on the provided configs. Options
    * `:conn_opts`: Defaults to `[client_settings: [initial_window_size: 8_000_000, max_frame_size: 8_000_000]]`, a larger default
      window size ensures that the number of packages exchanges is smaller, thus speeding up the requests by reducing the
      amount of networks round trip, with the cost of having larger packages reaching the server per connection.
      Check [Mint.HTTP.connect()/4](https://hexdocs.pm/mint/Mint.HTTP.html#connect/4) for additional configs.
  """
  @impl true
  def connect(channel, opts \\ []) do
    IO.inspect(:connect)
    # Added :config_options to facilitate testing.
    {config_opts, opts} = Keyword.pop(opts, :config_options, [])
    module_opts = Application.get_env(:grpc, __MODULE__, config_opts)

    {config_opts, _opts} = Keyword.pop(opts, :conn_opts, [])

    opts = conn_opts(channel, config_opts)
    pool_key = get_pool_key(channel)

    # Process.flag(:trap_exit, true)

    Finch.start_link(
      name: @finch_instance_name,
      pools: %{
        pool_key =>
          [
            protocols: [:http2],
            conn_opts: opts
          ]
          |> Keyword.merge(module_opts)
      }
    )
    |> case do
      {:ok, pid} ->
        {:ok, %{channel | adapter_payload: %{conn_pid: pid}}}

      {:error, {:already_started, pid}} ->
        {:ok, %{channel | adapter_payload: %{conn_pid: pid}}}

      error ->
        {:error, "Error while opening connection: #{inspect(error)}"}
    end
  end

  @impl true
  def disconnect(%{adapter_payload: %{conn_pid: pid}} = channel)
      when is_pid(pid) do
    IO.inspect(:disconnect)
    pool_key = get_pool_key(channel)

    Finch.stop_pool(@finch_instance_name, pool_key)

    {:ok, %{channel | adapter_payload: nil}}
  end

  def disconnect(%{adapter_payload: nil} = channel) do
    IO.inspect(:disconnect2)
    {:ok, channel}
  end

  @impl true
  def send_request(%{channel: %{adapter_payload: nil}}, _message, _opts),
    do: raise(ArgumentError, "Can't perform a request without a connection process")

  def send_request(stream, message, opts) do
    IO.inspect(:send_request)
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)
    path = get_full_path(stream)

    {:ok, stream_request_pid} = StreamRequestProcess.start_link(path, headers, data)

    GRPC.Client.Stream.put_payload(stream, :stream_request_pid, stream_request_pid)
  end

  @impl true
  def receive_data(
        %{
          channel: %{adapter_payload: %{conn_pid: _pid}}
        } = stream,
        opts
      ) do
    IO.inspect(:receive_data)
    do_receive_data(stream, stream.grpc_type, opts)
  end

  @impl true
  def send_headers(%{channel: %{adapter_payload: nil}}, _opts),
    do: raise("Can't start a client stream without a connection process")

  def send_headers(stream, opts) do
    IO.inspect(:send_headers)
    headers = GRPC.Transport.HTTP2.client_headers_without_reserved(stream, opts)
    {:ok, {body_stream, pid}} = CustomStream.start()

    path = get_full_path(stream)

    {:ok, stream_request_pid} =
      StreamRequestProcess.start_link(path, headers, {:stream, body_stream})

    GRPC.Client.Stream.put_payload(stream, :stream_request_pid, stream_request_pid)
    |> GRPC.Client.Stream.put_payload(:stream_state_pid, pid)
  end

  @impl true
  def send_data(
        %{
          channel: %{adapter_payload: %{conn_pid: _pid}},
          payload: %{stream_state_pid: stream_state_pid}
        } = stream,
        message,
        opts
      ) do
    IO.inspect(:send_data)
    {:ok, data, _} = GRPC.Message.to_data(message, opts)

    if opts[:send_end_stream] do
      # This synchronously sends the final data and closes the stream. Correct.
      IO.inspect(:send_end_stream)
      CustomStream.add_item(stream_state_pid, data)
      CustomStream.close(stream_state_pid)
      stream
    else
      CustomStream.add_item(stream_state_pid, data)
      stream
    end
  end

  @impl true
  def end_stream(
        %{
          channel: %{adapter_payload: %{conn_pid: _pid}},
          payload: %{stream_state_pid: stream_state_pid}
        } = stream
      ) do
    IO.inspect(:end_stream)
    CustomStream.close(stream_state_pid)
    stream
  end

  @impl true
  def cancel(stream) do
    %{
      channel: %{adapter_payload: %{conn_pid: _conn_pid}},
      payload: %{stream_request_pid: stream_request_pid} = payload
    } = stream

    IO.inspect(:cancel)

    if payload[:stream_state_pid] do
      CustomStream.close(payload[:stream_state_pid])
      GRPC.Client.Stream.put_payload(stream, :stream_state_pid, :closed)
    end

    StreamRequestProcess.close(stream_request_pid)

    :ok
  end

  defp conn_opts(%Channel{scheme: "https"} = channel, opts) do
    %Credential{ssl: ssl} = Map.get(channel, :cred) || %Credential{}

    Keyword.merge(@default_conn_opts, opts)
    |> Keyword.update(:transport_opts, ssl, fn x ->
      Keyword.merge(x, ssl)
    end)
  end

  defp conn_opts(_channel, opts) do
    Keyword.merge(@default_conn_opts, opts)
  end

  defp get_pool_key(%Channel{host: {:local, socket_path}, scheme: scheme, port: port}),
    do: {scheme, {:local, socket_path}, port}

  defp get_pool_key(%Channel{scheme: scheme, host: host, port: port}),
    do: "#{scheme}://#{host}:#{port}"

  defp do_receive_data(
         %{payload: %{stream_request_pid: stream_request_pid}} = stream,
         request_type,
         opts
       )
       when request_type in [:bidirectional_stream, :server_stream] do
    IO.inspect(:bidirectional_stream)

    response = response_data_stream(stream, stream_request_pid, opts)

    with {:headers, headers} <-
           Enum.at(response, 0) |> IO.inspect(label: :server_stream_response_headers) do
      IO.inspect(response)

      if opts[:return_headers] do
        {:ok, response, %{headers: headers}}
      else
        {:ok, response}
      end
    end
  end

  defp do_receive_data(
         %{payload: %{stream_request_pid: stream_request_pid}} = stream,
         request_type,
         opts
       )
       when request_type in [:client_stream, :unary] do
    IO.inspect(:client_stream)

    response =
      stream
      |> response_data_stream(stream_request_pid, opts)
      |> Enum.to_list()

    with {:headers, headers} <- Enum.at(response, 0),
         :ok <- check_for_error(response) do
      data = Keyword.fetch!(response, :ok)

      if opts[:return_headers] do
        {:ok, data, %{headers: headers, trailers: Keyword.get(response, :trailers)}}
      else
        {:ok, data}
      end
    end
  end

  defp response_data_stream(grpc_stream, stream_request_pid, opts) do
    IO.inspect(:response_data_stream)

    state = %{
      grpc_stream: grpc_stream,
      stream_request_pid: stream_request_pid,
      buffer: <<>>,
      opts: opts
    }

    Stream.resource(
      fn -> state end,
      fn acc ->
        case next_response(acc) do
          {nil, acc} -> {:halt, acc}
          {value, acc} -> {[value], acc}
        end
      end,
      fn acc ->
        IO.inspect("Closing stream resource")

        GenServer.stop(acc.stream_request_pid, :normal)
      end
    )
  end

  defp next_response(state) do
    state.stream_request_pid
    |> StreamRequestProcess.next_response()
    |> read_stream(state)
  end

  defp read_stream({header_or_trailer, headers}, state)
       when header_or_trailer in [:headers, :trailers] do
    IO.inspect(header_or_trailer)

    state = %{state | grpc_stream: check_compression(headers, state.grpc_stream)}

    if header_or_trailer == :headers || state.opts[:return_headers] do
      case parse_headers(headers) do
        {:ok, headers} -> {{header_or_trailer, headers}, state}
        error -> {error, state}
      end
    else
      next_response(state)
    end
  end

  defp read_stream({:data, data}, state) do
    IO.inspect(:data)

    case GRPC.Message.get_message(state.buffer <> data, state.grpc_stream.compressor) do
      {{_, message}, rest} ->
        IO.inspect(:message)
        reply = state.grpc_stream.codec.decode(message, state.grpc_stream.response_mod)
        new_state = Map.put(state, :buffer, rest)
        {{:ok, reply}, new_state}

      _ ->
        IO.inspect(:buffer)
        new_state = Map.put(state, :buffer, state.buffer <> data)
        next_response(new_state)
    end
  end

  defp read_stream({:error, _} = error, state) do
    IO.inspect(:error_ref)
    {error, state}
  end

  defp read_stream(:done, state) do
    IO.inspect(:done)
    {nil, state}
  end

  defp check_for_error(responses) do
    error = Keyword.get(responses, :error)

    if error, do: {:error, error}, else: :ok
  end

  defp get_full_path(%{channel: %{host: host, port: port, scheme: scheme}, path: path}) do
    "#{scheme}://#{host}:#{port}#{path}"
  end

  defp check_compression(headers, stream) do
    headers_map = Map.new(headers)
    encoding = headers_map["grpc-encoding"]

    if encoding do
      encoding = Enum.find(stream.accepted_compressors, nil, fn c -> c.name() == encoding end)
      Map.put(stream, :compressor, encoding)
    else
      stream
    end
  end

  defp parse_headers(headers) do
    IO.inspect(:parse_headers)
    headers = GRPC.Transport.HTTP2.decode_headers(headers)

    if headers["grpc-status"] do
      grpc_status = String.to_integer(headers["grpc-status"])

      if grpc_status == GRPC.Status.ok() do
        {:ok, headers}
      else
        {:error, %GRPC.RPCError{status: grpc_status, message: headers["grpc-message"]}}
      end
    else
      {:ok, headers}
    end
  end
end

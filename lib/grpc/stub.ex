defmodule GRPC.Stub do
  @moduledoc """
  A module acting as the interface for gRPC client.

  You can do everything in the client side via `GRPC.Stub`, including connecting,
  sending/receiving steaming or non-steaming requests, canceling calls and so on.

  A service is needed to define a stub:

      defmodule Greeter.Service do
        use GRPC.Service, name: "ping"

        rpc :SayHello, Request, Reply
        rpc :SayGoodbye, stream(Request), stream(Reply)
      end

      defmodule Greeter.Stub do
        use GRPC.Stub, service: Greeter.Service
      end

  so that functions `say_hello/2` and `say_goodbye/1` will be generated for you:

      # Unary call
      {:ok, reply} = Greeter.Stub.say_hello(channel, request)

      # Streaming call
      stream = Greeter.Stub.say_goodbye(channel)
      GRPC.Stub.send_request(stream, request, end_stream: true)
      {:ok, reply_enum} = GRPC.Stub.recv(stream)
      replies = Enum.map(reply_enum, fn({:ok, reply}) -> reply end)

  Note that streaming calls are very different with unary calls. If request is
  streaming, the RPC function only accepts channel as argument and returns a
  `GRPC.Client.Stream`. You can send streaming requests one by one via `send_request/3`,
  then use `recv/1` to receive the reply. And if the reply is streaming, `recv/1`
  returns a `Stream`.

  You can refer to `call/6` for doc of your RPC functions.
  """
  alias GRPC.Channel
  @insecure_scheme "http"
  @secure_scheme "https"
  @canceled_error GRPC.RPCError.exception(GRPC.Status.cancelled(), "The operation was cancelled")

  # 5 seconds
  @default_timeout 5_000_000

  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)

      Enum.each(service_mod.__rpc_calls__, fn {name, {_, req_stream}, {_, res_stream}} = rpc ->
        func_name = name |> to_string |> Macro.underscore()
        path = "/#{service_name}/#{name}"

        if req_stream do
          def unquote(String.to_atom(func_name))(channel, opts \\ []) do
            GRPC.Stub.call(
              unquote(service_mod),
              unquote(Macro.escape(rpc)),
              unquote(path),
              channel,
              nil,
              opts
            )
          end
        else
          def unquote(String.to_atom(func_name))(channel, request, opts \\ []) do
            GRPC.Stub.call(
              unquote(service_mod),
              unquote(Macro.escape(rpc)),
              unquote(path),
              channel,
              request,
              opts
            )
          end
        end
      end)
    end
  end

  @doc """
  Establish a connection with gRPC server and return `GRPC.Channel` needed for
  sending requests.

  ## Examples

      iex> GRPC.Stub.connect("localhost:50051")
      {:ok, channel}

  ## Options

    * `:cred` - a `GRPC.Credential` used to indicate it's a secure connection.
      An insecure connection will be created without this option.
    * `:adapter` - custom client adapter
  """
  @spec connect(String.t(), Keyword.t()) :: {:ok, GRPC.Channel.t()} | {:error, any}
  def connect(addr, opts \\ []) when is_binary(addr) and is_list(opts) do
    [host, port] = String.split(addr, ":")
    connect(host, port, opts)
  end

  @spec connect(String.t(), binary | non_neg_integer, keyword) ::
          {:ok, Channel.t()} | {:error, any}
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end

  def connect(host, port, opts) when is_integer(port) do
    adapter =
      Keyword.get(
        opts,
        :adapter,
        Application.get_env(:grpc, :http2_client_adapter, GRPC.Adapter.Gun)
      )

    cred = Keyword.get(opts, :cred)
    scheme = if cred, do: @secure_scheme, else: @insecure_scheme

    %Channel{host: host, port: port, scheme: scheme, cred: cred, adapter: adapter}
    |> adapter.connect(opts[:adapter_opts])
  end

  @doc """
  The actual function invoked when invoking a rpc function.

  ## Returns

    * Unary calls. `{:ok, reply} | {:ok, headers_map} | {:error, error}`
    * Client streaming. A `GRPC.Client.Stream`
    * Server streaming. `{:ok, Enumerable.t} | {:ok, Enumerable.t, trailers_map} | {:error, error}`

  ## Options

    * `:timeout` - request timeout
    * `:deadline` - when the request is timeout, will override timeout
    * `:metadata` - a map, your custom metadata
    * `:return_headers` - default is false. When it's true, a three elem tuple will be returned
      with the last elem being a map of headers `%{headers: headers, trailers: trailers}`(unary) or
      `%{headers: headers}`(server streaming)
  """
  @spec call(atom, tuple, String.t(), GRPC.Channel, struct | nil, keyword) ::
          {:ok, struct}
          | {:ok, struct, map}
          | GRPC.Client.Stream.t()
          | {:ok, Enumerable.t()}
          | {:error, GRPC.RPCError.t()}
  def call(service_mod, rpc, path, channel, request, opts) do
    {_, {req_mod, req_stream}, {res_mod, res_stream}} = rpc
    marshal = fn req -> service_mod.marshal(req_mod, req) end
    unmarshal = fn res -> service_mod.unmarshal(res_mod, res) end

    stream = %GRPC.Client.Stream{
      marshal: marshal,
      unmarshal: unmarshal,
      path: path,
      req_stream: req_stream,
      res_stream: res_stream,
      channel: channel
    }

    opts = parse_req_opts(opts)

    do_call(req_stream, res_stream, stream, request, opts)
  end

  defp do_call(false, _, %{marshal: marshal, channel: channel} = stream, request, opts) do
    message = marshal.(request)

    stream
    |> channel.adapter.send_request(message, opts)
    |> recv(opts)
  end

  defp do_call(true, _, %{channel: channel} = stream, _, opts) do
    channel.adapter.send_headers(stream, opts)
  end

  @doc """
  DEPRECATED. Use `send_request/3` instead
  """
  @deprecated "Use send_request/3 instead"
  def stream_send(stream, request, opts \\ []) do
    send_request(stream, request, opts)
  end

  @doc """
  Send streaming requests.

  The last request can be sent with `:end_stream` option, or you can call `end_stream/1`
  to send a frame with END_STREAM flag to end the stream.

  ## Options

    * `:end_stream` - indicates it's the last one request, then the stream will be in
      half_closed state. Default is false.
  """
  @spec send_request(GRPC.Client.Stream.t(), struct, Keyword.t()) :: GRPC.Client.Stream.t()
  def send_request(%{marshal: marshal, channel: channel} = stream, request, opts \\ []) do
    message = marshal.(request)
    send_end_stream = Keyword.get(opts, :end_stream, false)
    channel.adapter.send_data(stream, message, send_end_stream: send_end_stream)
  end

  @doc """
  Send END_STREAM frame to end the stream.

  The stream will be in half_closed state after this is called.

  ## Examples

      iex> stream = GRPC.Stub.send_request(stream, request)
      iex> GRPC.Stub.end_stream(stream)
  """
  @spec end_stream(GRPC.Client.Stream.t()) :: GRPC.Client.Stream.t()
  def end_stream(%{channel: channel} = stream) do
    channel.adapter.end_stream(stream)
  end

  @doc """
  Cancel a stream in a streaming client.

  After that, callings to `recv/2` will return a CANCEL error.
  """
  def cancel(%{channel: channel, payload: payload} = stream) do
    case channel.adapter.cancel(channel.adapter_payload, payload) do
      :ok -> %{stream | canceled: true}
      other -> other
    end
  end

  @doc """
  Receive replies when requests are streaming.

  * If the reply is not streaming, a normal reply struct will be returned
  * If the reply is streaming, a enumerable `Stream` will be returned.
    You can use `Enum` to fetch further replies or `Stream` to manipulate it.
    Each item in the `Enumrable` is a tuple `{:ok, reply}` or `{:error, error}`.
    When `:return_headers` is true, the last item in the `Enumrable` will be
    `{:trailers, map}`

  ## Examples

      # Reply is not streaming
      {:ok, reply} = GRPC.Stub.recv(stream)

      # Reply is streaming
      {:ok, enum} = GRPC.Stub.recv(stream)
      replies = Enum.map(enum, fn({:ok, reply}) -> reply end)

  ## Options

    * `:timeout` - request timeout
    * `:deadline` - when the request is timeout, will override timeout
    * `:return_headers` - when true, headers will be returned.
  """
  @spec recv(GRPC.Client.Stream.t(), keyword | map) ::
          {:ok, struct} | {:ok, struct, map} | Enumerable.t() | {:error, any}
  def recv(stream, opts \\ [])

  def recv(%{canceled: true}, _) do
    {:error, @canceled_error}
  end

  def recv(stream, opts) when is_list(opts) do
    recv(stream, parse_recv_opts(opts))
  end

  def recv(%{res_stream: true, channel: channel, payload: payload} = stream, opts)
      when is_map(opts) do
    case recv_headers(channel.adapter, channel.adapter_payload, payload, opts) do
      {:ok, headers} ->
        res_enum = response_stream(stream, opts)

        if opts[:return_headers] do
          {:ok, res_enum, %{headers: headers}}
        else
          {:ok, res_enum}
        end

      {:error, reason} ->
        cancel(stream)
        {:error, reason}
    end
  end

  def recv(%{payload: payload, unmarshal: unmarshal, channel: channel} = stream, opts) do
    with {:ok, headers} <- recv_headers(channel.adapter, channel.adapter_payload, payload, opts),
         {:ok, body, trailers} <-
           recv_body(channel.adapter, channel.adapter_payload, payload, opts) do
      {status, msg} = parse_response(body, trailers, unmarshal)

      if opts[:return_headers] do
        {status, msg, %{headers: headers, trailers: trailers}}
      else
        {status, msg}
      end
    else
      error = {:error, _} ->
        cancel(stream)
        error
    end
  end

  defp recv_headers(adapter, conn_payload, stream_payload, opts) do
    case adapter.recv_headers(conn_payload, stream_payload, opts) do
      {:ok, headers} ->
        {:ok, GRPC.Transport.HTTP2.decode_headers(headers)}

      other ->
        other
    end
  end

  defp recv_body(adapter, conn_payload, stream_payload, opts) do
    recv_body(adapter, conn_payload, stream_payload, "", opts)
  end

  defp recv_body(adapter, conn_payload, stream_payload, acc, opts) do
    case adapter.recv_data_or_trailers(conn_payload, stream_payload, opts) do
      {:data, data} ->
        recv_body(adapter, conn_payload, stream_payload, <<acc::binary, data::binary>>, opts)

      {:trailers, trailers} ->
        {:ok, acc, GRPC.Transport.HTTP2.decode_headers(trailers)}
    end
  end

  defp parse_response(data, trailers, unmarshal) do
    case parse_trailers(trailers) do
      :ok ->
        result = decode_data(data, unmarshal)
        {:ok, result}

      error ->
        error
    end
  end

  defp parse_trailers(trailers) do
    status = String.to_integer(trailers["grpc-status"])

    if status == GRPC.Status.ok() do
      :ok
    else
      {:error, %GRPC.RPCError{status: status, message: trailers["grpc-message"]}}
    end
  end

  defp response_stream(%{unmarshal: unmarshal, channel: channel, payload: payload} = stream, opts) do
    enum =
      Stream.unfold(%{buffer: "", message_length: -1, fin: false}, fn
        %{fin: true} ->
          nil

        acc ->
          case channel.adapter.recv_data_or_trailers(channel.adapter_payload, payload, opts) do
            {:data, data} ->
              if GRPC.Message.complete?(data) do
                reply = decode_data(data, unmarshal)
                {{:ok, reply}, %{buffer: "", message_length: -1}}
              else
                handle_incomplete_data(acc, data, unmarshal)
              end

            {:trailers, trailers} ->
              trailers = GRPC.Transport.HTTP2.decode_headers(trailers)

              case parse_trailers(trailers) do
                :ok ->
                  if opts[:return_headers] do
                    {{:trailers, trailers}, Map.put(acc, :fin, true)}
                  end

                error ->
                  {error, Map.put(acc, :fin, true)}
              end

            error = {:error, _} ->
              cancel(stream)
              {error, Map.put(acc, :fin, true)}
          end
      end)

    Stream.reject(enum, &match?(:skip, &1))
  end

  defp handle_incomplete_data(%{buffer: ""} = acc, data, _) do
    new_acc =
      acc
      |> Map.put(:buffer, data)
      |> Map.put(:message_length, GRPC.Message.message_length(data))

    {:skip, new_acc}
  end

  defp handle_incomplete_data(%{buffer: buffer, message_length: ml}, data, unmarshal)
       when byte_size(buffer) + byte_size(data) - 5 == ml and ml > 0 do
    final_buffer = buffer <> data

    reply = decode_data(final_buffer, unmarshal)

    {{:ok, reply}, %{buffer: "", message_length: -1}}
  end

  defp handle_incomplete_data(%{buffer: buffer} = acc, data, _) do
    {:skip, Map.put(acc, :buffer, buffer <> data)}
  end

  defp decode_data(data, unmarshal) do
    data
    |> GRPC.Message.from_data()
    |> unmarshal.()
  end

  defp parse_req_opts(list) when is_list(list) do
    parse_req_opts(list, %{timeout: @default_timeout})
  end

  defp parse_req_opts([{:timeout, timeout} | t], acc) do
    parse_req_opts(t, Map.put(acc, :timeout, timeout))
  end

  defp parse_req_opts([{:deadline, deadline} | t], acc) do
    parse_req_opts(t, Map.put(acc, :timeout, GRPC.TimeUtils.to_relative(deadline)))
  end

  defp parse_req_opts([{:compressor, compressor} | t], acc) do
    parse_req_opts(t, Map.put(acc, :compressor, compressor))
  end

  defp parse_req_opts([{:grpc_encoding, grpc_encoding} | t], acc) do
    parse_req_opts(t, Map.put(acc, :grpc_encoding, grpc_encoding))
  end

  defp parse_req_opts([{:metadata, metadata} | t], acc) do
    parse_req_opts(t, Map.put(acc, :metadata, metadata))
  end

  defp parse_req_opts([{:content_type, content_type} | t], acc) do
    parse_req_opts(t, Map.put(acc, :content_type, content_type))
  end

  defp parse_req_opts([{:return_headers, return_headers} | t], acc) do
    parse_req_opts(t, Map.put(acc, :return_headers, return_headers))
  end

  defp parse_req_opts([{key, _} | _], _) do
    raise ArgumentError, "option #{inspect(key)} is not supported"
  end

  defp parse_req_opts(_, acc), do: acc

  defp parse_recv_opts(list) when is_list(list) do
    parse_recv_opts(list, %{timeout: @default_timeout})
  end

  defp parse_recv_opts([{:timeout, timeout} | t], acc) do
    parse_recv_opts(t, Map.put(acc, :timeout, timeout))
  end

  defp parse_recv_opts([{:deadline, deadline} | t], acc) do
    parse_recv_opts(t, Map.put(acc, :deadline, GRPC.TimeUtils.to_relative(deadline)))
  end

  defp parse_recv_opts([{:return_headers, return_headers} | t], acc) do
    parse_recv_opts(t, Map.put(acc, :return_headers, return_headers))
  end

  defp parse_recv_opts([{key, _} | _], _) do
    raise ArgumentError, "option #{inspect(key)} is not supported"
  end

  defp parse_recv_opts(_, acc), do: acc
end

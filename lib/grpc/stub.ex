defmodule GRPC.Stub do
  @moduledoc """
  A module acting as the interface for gRPC client.

  You can do everything in the client side via `GRPC.Stub`, including connecting,
  sending or receiving steaming or non-steaming requests.

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
      iex> reply = Greeter.Stub.say_hello(channel, request)
      # Streaming call
      iex> stream = Greeter.Stub.say_goodbye(channel)
      iex> GRPC.Stub.stream_send(stream, request, end_stream: true)
      iex> reply_enum = GRPC.Stub.recv(stream)
      iex> reply = Enum.map reply_enum, &(&1)

  Note that streaming calls are very different with unary calls. If request is
  streaming, the RPC function only accepts channel as argument and returns a stream
  `GRPC.Client.Stream`. You can send streaming requests one by one via `stream_send/3`,
  then use `recv/1` to receive the reply. And if the reply is streaming, `recv/1`
  returns a `Stream`.
  """
  alias GRPC.Channel
  @insecure_scheme "http"
  @secure_scheme "https"

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

    * `:cred` - a `GRPC.Credential` used to indicate it's a secure connection,
                an insecure connection will be created without this option.
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
    adapter = Keyword.get(opts, :adapter, Application.get_env(:grpc, :http2_client_adapter, GRPC.Adapter.Chatterbox.Client))
    cred = Keyword.get(opts, :cred)
    scheme = if cred, do: @secure_scheme, else: @insecure_scheme

    %Channel{host: host, port: port, scheme: scheme, cred: cred, adapter: adapter}
    |> adapter.connect(opts[:adapter_opts])
  end

  @doc false
  @spec call(atom, tuple, String.t(), GRPC.Channel, struct | nil, keyword) :: any
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

    send_request(req_stream, res_stream, stream, request, opts)
  end

  defp send_request(
         false,
         false,
         %{marshal: marshal, unmarshal: unmarshal, channel: channel} = stream,
         request,
         opts
       ) do
    message = marshal.(request)

    case channel.adapter.unary(stream, message, opts) do
      {:ok, headers, data} ->
        parse_response(headers, data, unmarshal)

      other ->
        other
    end
  end

  defp send_request(false, true, %{marshal: marshal, channel: channel} = stream, request, opts) do
    message = marshal.(request)
    {:ok, new_stream} = channel.adapter.send_request(stream, message, opts)
    response_stream(new_stream, opts)
  end

  defp send_request(true, false, %{channel: channel} = stream, _, opts) do
    {:ok, stream} = channel.adapter.send_header(stream, opts)
    stream
  end

  defp send_request(true, true, %{channel: channel} = stream, _, opts) do
    {:ok, stream} = channel.adapter.send_header(stream, opts)
    stream
  end

  @doc """
  Send streaming requests.

  The last request must be sent with `:end_stream` option.

  ## Examples

      iex> GRPC.Stub.stream_send(stream, request, opts)
      :ok

  ## Options

    * `:end_stream` - indicates it's the last one request
  """
  @spec stream_send(GRPC.Client.Stream, struct, keyword) :: :ok
  def stream_send(%{marshal: marshal, channel: channel} = stream, request, opts \\ []) do
    message = marshal.(request)
    send_end_stream = Keyword.get(opts, :end_stream, false)
    channel.adapter.send_body(stream, message, send_end_stream: send_end_stream)
    :ok
  end

  @doc """
  Receive replies when requests are streaming.

  * If the reply is not streaming, a normal reply struct will be returned
  * If the reply is streaming, a enumerable `Stream` will be returned.

  ## Examples

      # Reply is not streaming
      iex> {:ok, reply} = GRPC.Stub.recv(stream)

      # Reply is streaming
      iex> enum = GRPC.Stub.recv(stream)
      iex> replies_stream = Enum.map enum, &(&1)
  """
  @spec recv(GRPC.Client.Stream.t(), keyword) :: {:ok, struct} | Enumerable.t() | {:error, any}
  def recv(stream, opts \\ [])

  def recv(%{res_stream: true} = stream, opts) do
    response_stream(stream, opts)
  end

  def recv(%{unmarshal: unmarshal, channel: channel} = stream, opts) do
    {:ok, headers, data} = channel.adapter.recv_end(stream, opts)
    parse_response(headers, data, unmarshal)
  end

  defp parse_response(headers, data, unmarshal) do
    {_, status} = Enum.find(headers, nil, fn header -> elem(header, 0) == "grpc-status" end)
    status = String.to_integer(status)

    if status != GRPC.Status.ok() do
      {_, message} = Enum.find(headers, nil, fn header -> elem(header, 0) == "grpc-message" end)
      {:error, %GRPC.RPCError{status: status, message: message}}
    else
      result =
        data
        |> GRPC.Message.from_data()
        |> unmarshal.()

      {:ok, result}
    end
  end

  defp response_stream(%{unmarshal: unmarshal, channel: channel} = stream, opts) do
    resp_stream =
      Stream.unfold(%{buffer: :empty, message_length: -1}, fn acc ->
        case channel.adapter.recv(stream, opts) do
          {:data, data} ->
            if GRPC.Message.complete?(data) do
              reply = data |> GRPC.Message.from_data() |> unmarshal.()
              {reply, acc}
            else
              case acc do
                %{buffer: :empty} ->
                  {
                    :skip,
                    Map.merge(acc, %{
                      buffer: data,
                      message_length: GRPC.Message.message_length(data)
                    })
                  }

                %{buffer: buffer, message_length: ml}
                when byte_size(buffer) + byte_size(data) - 5 == ml ->
                  final_buffer = buffer <> data

                  reply =
                    final_buffer
                    |> GRPC.Message.from_data()
                    |> unmarshal.()

                  {
                    reply,
                    Map.merge(acc, %{buffer: :empty, message_length: -1})
                  }

                %{buffer: buffer} ->
                  next_buffer = buffer <> data
                  {:skip, Map.put(acc, :buffer, next_buffer)}
              end
            end

          {:end_stream, _resp} ->
            nil

          other ->
            other
        end
      end)

    Stream.reject(resp_stream, &match?(:skip, &1))
  end
end

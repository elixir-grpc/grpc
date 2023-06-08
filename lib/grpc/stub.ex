defmodule GRPC.Stub do
  @moduledoc """
  A module acting as the interface for gRPC client.

  You can do everything in the client side via `GRPC.Stub`, including connecting,
  sending/receiving streaming or non-streaming requests, canceling calls and so on.

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
  # 10 seconds
  @default_timeout 10000

  @type receive_data_return ::
          {:ok, struct()}
          | {:ok, struct(), map()}
          | {:ok, Enumerable.t()}
          | {:ok, Enumerable.t(), map()}

  @type rpc_return ::
          GRPC.Client.Stream.t()
          | {:error, GRPC.RPCError.t()}
          | receive_data_return

  require Logger

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      service_mod = opts[:service]
      service_name = service_mod.__meta__(:name)

      Enum.each(service_mod.__rpc_calls__, fn {name, {_, req_stream}, {_, res_stream}} = rpc ->
        func_name = name |> to_string |> Macro.underscore()
        path = "/#{service_name}/#{name}"
        grpc_type = GRPC.Service.grpc_type(rpc)

        stream = %GRPC.Client.Stream{
          service_name: service_name,
          method_name: to_string(name),
          grpc_type: grpc_type,
          path: path,
          rpc: put_elem(rpc, 0, func_name),
          server_stream: res_stream
        }

        if req_stream do
          def unquote(String.to_atom(func_name))(channel, opts \\ []) do
            GRPC.Stub.call(
              unquote(service_mod),
              unquote(Macro.escape(rpc)),
              %{unquote(Macro.escape(stream)) | channel: channel},
              nil,
              opts
            )
          end
        else
          def unquote(String.to_atom(func_name))(channel, request, opts \\ []) do
            GRPC.Stub.call(
              unquote(service_mod),
              unquote(Macro.escape(rpc)),
              %{unquote(Macro.escape(stream)) | channel: channel},
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

      iex> GRPC.Stub.connect("localhost:50051", accepted_compressors: [GRPC.Compressor.Gzip])
      {:ok, channel}

      iex> GRPC.Stub.connect("/paht/to/unix.sock")
      {:ok, channel}

  ## Options

    * `:cred` - a `GRPC.Credential` used to indicate it's a secure connection.
      An insecure connection will be created without this option.
    * `:adapter` - custom client adapter
    * `:interceptors` - client interceptors
    * `:codec` - client will use this to encode and decode binary message
    * `:compressor` - the client will use this to compress requests and decompress responses. If this is set, accepted_compressors
        will be appended also, so this can be used safely without `:accepted_compressors`.
    * `:accepted_compressors` - tell servers accepted compressors, this can be used without `:compressor`
    * `:headers` - headers to attach to each request
  """
  @spec connect(String.t(), keyword()) :: {:ok, Channel.t()} | {:error, any()}
  def connect(addr, opts \\ []) when is_binary(addr) and is_list(opts) do
    {host, port} =
      case String.split(addr, ":") do
        [host, port] -> {host, port}
        [socket_path] -> {{:local, socket_path}, 0}
      end

    connect(host, port, opts)
  end

  @spec connect(String.t(), binary() | non_neg_integer(), keyword()) ::
          {:ok, Channel.t()} | {:error, any()}
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end

  def connect(host, port, opts) when is_integer(port) do
    if Application.get_env(:grpc, :http2_client_adapter) do
      raise "the :http2_client_adapter config key has been deprecated.\
      The currently supported way is to configure it\
      through the :adapter option for GRPC.Stub.connect/3"
    end

    adapter = Keyword.get(opts, :adapter) || GRPC.Client.Adapters.Gun

    cred = Keyword.get(opts, :cred)
    scheme = if cred, do: @secure_scheme, else: @insecure_scheme
    interceptors = (Keyword.get(opts, :interceptors) || []) |> init_interceptors
    codec = Keyword.get(opts, :codec) || GRPC.Codec.Proto
    compressor = Keyword.get(opts, :compressor)
    accepted_compressors = Keyword.get(opts, :accepted_compressors) || []
    headers = Keyword.get(opts, :headers) || []

    accepted_compressors =
      if compressor do
        Enum.uniq([compressor | accepted_compressors])
      else
        accepted_compressors
      end

    adapter_opts = opts[:adapter_opts] || []

    unless is_list(adapter_opts) do
      raise ArgumentError, ":adapter_opts must be a keyword list if present"
    end

    %Channel{
      host: host,
      port: port,
      scheme: scheme,
      cred: cred,
      adapter: adapter,
      interceptors: interceptors,
      codec: codec,
      compressor: compressor,
      accepted_compressors: accepted_compressors,
      headers: headers
    }
    |> adapter.connect(adapter_opts)
  end

  def retry_timeout(curr) when curr < 11 do
    timeout =
      if curr < 11 do
        :math.pow(1.6, curr - 1) * 1000
      else
        120_000
      end

    jitter = (:rand.uniform_real() - 0.5) / 2.5

    round(timeout + jitter * timeout)
  end

  defp init_interceptors(interceptors) do
    Enum.map(interceptors, fn
      {interceptor, opts} -> {interceptor, interceptor.init(opts)}
      interceptor -> {interceptor, interceptor.init([])}
    end)
  end

  @doc """
  Disconnects the adapter and frees any resources the adapter is consuming
  """
  @spec disconnect(Channel.t()) :: {:ok, Channel.t()} | {:error, any()}
  def disconnect(%Channel{adapter: adapter} = channel) do
    adapter.disconnect(channel)
  end

  @doc false
  #  #  The actual function invoked when invoking an RPC function.
  #
  #  Returns
  #
  #    * Unary calls. `{:ok, reply} | {:ok, headers_map} | {:error, error}`
  #    * Client streaming. A `GRPC.Client.Stream`
  #    * Server streaming. `{:ok, Enumerable.t} | {:ok, Enumerable.t, trailers_map} | {:error, error}`
  #
  #  Options
  #
  #    * `:timeout` - request timeout. Default is 10s for unary calls and `:infinity` for
  #      client or server streaming calls
  #    * `:deadline` - when the request is timeout, will override timeout
  #    * `:metadata` - a map, your custom metadata
  #    * `:return_headers` - default is false. When it's true, a three elem tuple will be returned
  #      with the last elem being a map of headers `%{headers: headers, trailers: trailers}`(unary) or
  #      `%{headers: headers}`(server streaming)
  def call(_service_mod, rpc, %{channel: channel} = stream, request, opts) do
    {_, {req_mod, req_stream}, {res_mod, response_stream}} = rpc

    stream = %{stream | request_mod: req_mod, response_mod: res_mod}

    opts =
      if req_stream || response_stream do
        parse_req_opts([{:timeout, :infinity} | opts])
      else
        parse_req_opts([{:timeout, @default_timeout} | opts])
      end

    compressor = Keyword.get(opts, :compressor, channel.compressor)
    accepted_compressors = Keyword.get(opts, :accepted_compressors, [])

    accepted_compressors =
      if compressor do
        Enum.uniq([compressor | accepted_compressors])
      else
        accepted_compressors
      end

    stream = %{
      stream
      | codec: Keyword.get(opts, :codec, channel.codec),
        compressor: Keyword.get(opts, :compressor, channel.compressor),
        accepted_compressors: accepted_compressors
    }

    GRPC.Telemetry.client_span(stream, fn ->
      do_call(req_stream, stream, request, opts)
    end)
  end

  defp do_call(
         false,
         %{channel: channel} = stream,
         request,
         opts
       ) do
    last = fn %{codec: codec, compressor: compressor} = s, _ ->
      message = codec.encode(request)
      opts = Keyword.put(opts, :compressor, compressor)

      s
      |> channel.adapter.send_request(message, opts)
      |> recv(opts)
    end

    next =
      Enum.reduce(channel.interceptors, last, fn {interceptor, opts}, acc ->
        fn s, r -> interceptor.call(s, r, acc, opts) end
      end)

    next.(stream, request)
  end

  defp do_call(true, %{channel: channel} = stream, req, opts) do
    last = fn s, _ ->
      channel.adapter.send_headers(s, opts)
    end

    next =
      Enum.reduce(channel.interceptors, last, fn {interceptor, opts}, acc ->
        fn s, r -> interceptor.call(s, r, acc, opts) end
      end)

    next.(stream, req)
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
  @spec send_request(GRPC.Client.Stream.t(), struct, keyword()) :: GRPC.Client.Stream.t()
  def send_request(%{__interface__: interface} = stream, request, opts \\ []) do
    interface[:send_request].(stream, request, opts)
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
  def cancel(%{channel: channel} = stream) do
    case channel.adapter.cancel(stream) do
      :ok -> %{stream | canceled: true}
      other -> other
    end
  end

  @doc """
  Receive replies when requests are streaming.

  * If the reply is not streaming, a normal reply struct will be returned
  * If the reply is streaming, a enumerable `Stream` will be returned.
    You can use `Enum` to fetch further replies or `Stream` to manipulate it.
    Each item in the `Enumerable` is a tuple `{:ok, reply}` or `{:error, error}`.
    When `:return_headers` is true, the last item in the `Enumerable` will be
    `{:trailers, map}`

  ## Examples

      # Reply is not streaming
      {:ok, reply} = GRPC.Stub.recv(stream)

      # Reply is streaming
      {:ok, ex_stream} = GRPC.Stub.recv(stream)
      replies = Enum.map(ex_stream, fn({:ok, reply}) -> reply end)

  ## Options

    * `:timeout` - request timeout
    * `:deadline` - when the request is timeout, will override timeout
    * `:return_headers` - when true, headers will be returned.

  ## Stream behavior
  We build the Stream struct using `Stream.unfold/2`.

  The unfold function is built in such a way that  - for both adapters - the accumulator is a map used to find the
  `connection_stream`process and the `next_fun` argument is a function that reads directly from the `connection_stream`
  that is producing data.
  Every time we execute `next_fun` we read a chunk of data. This means that `next_fun` will have the side effect of updating the state of the `connection_stream` process, removing the chunk of data that's being read from the underlying `GenServer`'s state.


  ## Examples

      iex> ex_stream |> Stream.take(1) |> Enum.to_list()
      [1]
      iex> ex_stream |> Enum.to_list()
      [2, 3]
      iex> ex_stream |> Enum.to_list()
      []
  """
  @spec recv(GRPC.Client.Stream.t(), keyword()) ::
          {:ok, struct()}
          | {:ok, struct(), map()}
          | {:ok, Enumerable.t()}
          | {:ok, Enumerable.t(), map()}
          | {:error, any()}
  def recv(stream, opts \\ [])

  def recv(%{canceled: true}, _) do
    {:error, @canceled_error}
  end

  def recv(%{__interface__: interface} = stream, opts) do
    opts =
      if is_list(opts) do
        parse_recv_opts(Keyword.put_new(opts, :timeout, @default_timeout))
      else
        opts
      end

    interface[:receive_data].(stream, opts)
  end

  @valid_req_opts [
    :timeout,
    :deadline,
    :compressor,
    :accepted_compressors,
    :grpc_encoding,
    :metadata,
    :codec,
    :return_headers
  ]
  defp parse_req_opts(opts) when is_list(opts) do
    # Map.new is used so we can keep the last value
    # passed for a given key
    opts
    |> Map.new(fn
      {:deadline, deadline} ->
        {:timeout, GRPC.TimeUtils.to_relative(deadline)}

      {key, value} when key in @valid_req_opts ->
        {key, value}

      {key, _} ->
        raise ArgumentError, "option #{inspect(key)} is not supported"
    end)
    |> Map.to_list()
  end

  defp parse_recv_opts(list) when is_list(list) do
    # Map.new is used so we can keep the last value
    # passed for a given key

    list
    |> Map.new(fn
      {:deadline, deadline} ->
        {:deadline, GRPC.TimeUtils.to_relative(deadline)}

      {key, _} when key not in @valid_req_opts ->
        raise ArgumentError, "option #{inspect(key)} is not supported"

      kv ->
        kv
    end)
    |> Map.to_list()
  end
end

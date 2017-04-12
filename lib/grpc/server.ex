defmodule GRPC.Server do
  @moduledoc """
  A gRPC server which handles requests by calling user-defined functions.

  You should pass a `GRPC.Service` in when using the module:

      defmodule Greeter.Service do
        use GRPC.Service, name: "ping"

        rpc :SayHello, Request, Reply
        rpc :SayGoodbye, stream(Request), stream(Reply)
      end

      defmodule Greeter.Server do
        use GRPC.Server, service: Greeter.Service

        def say_hello(request, _stream) do
          Reply.new(message: "Hello")
        end

        def say_goodbye(request_enum, stream) do
          requests = Enum.map request_enum, &(&1)
          GRPC.Server.stream_send(stream, reply1)
          GRPC.Server.stream_send(stream, reply2)
        end
      end

  Your functions should accept a client request and a `GRPC.Server.Stream`.
  The client request will be a `Stream` of requests if it's streaming.
  If a reply is streaming, you need to call `stream_send/2` to send requests
  one by one instead of returning reply in the end.

  A server can be started and stoped using:

      {:ok, _, port} = GRPC.Server.start(Greeter.Server, 50051)
      :ok = GRPC.Server.stop(Greeter.Server)
  """

  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      Enum.each service_mod.__rpc_calls__, fn ({name, _, _} = rpc) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def __call_rpc__(unquote(path), stream) do
          GRPC.Server.call(unquote(service_mod), stream, unquote(Macro.escape(rpc)), String.to_atom(unquote(func_name)))
        end
        def __call_rpc(_, stream), do: {:error, stream, "Error"}
      end
    end
  end

  @doc false
  @spec call(atom, GRPC.Server.Stream.t,
    tuple, atom) :: {:ok, GRPC.Server.Stream.t, struct} | {:ok, struct}
  def call(service_mod, stream, {_, {req_mod, req_stream}, {res_mod, res_stream}} = _rpc, func_name) do
    marshal_func = fn(res) -> service_mod.marshal(res_mod, res) end
    unmarshal_func = fn(req) -> service_mod.unmarshal(req_mod, req) end
    stream = %{stream | marshal: marshal_func, unmarshal: unmarshal_func}

    handle_request(req_stream, res_stream, stream, func_name)
  end

  defp handle_request(false = req_stream, res_stream, %{unmarshal: unmarshal, adapter: adapter} = stream, func_name) do
    {:ok, data, stream} = adapter.read_body(stream)
    message = GRPC.Message.from_data(data)
    request = unmarshal.(message)
    handle_request(req_stream, res_stream, stream, func_name, request)
  end
  defp handle_request(true = req_stream, res_stream, %{unmarshal: unmarshal, adapter: adapter} = stream, func_name) do
    reading_stream = adapter.reading_stream(stream, fn (data) ->
      data
      |> GRPC.Message.from_data
      |> unmarshal.()
    end)
    handle_request(req_stream, res_stream, stream, func_name, reading_stream)
  end

  defp handle_request(false, false, %{server: server_mod} = stream, func_name, request) do
    response = apply(server_mod, func_name, [request, stream])
    {:ok, stream, response}
  end
  defp handle_request(false, true, %{server: server_mod} = stream, func_name, request) do
    apply(server_mod, func_name, [request, stream])
    {:ok, stream}
  end
  defp handle_request(true, false, %{server: server_mod} = stream, func_name, req_stream) do
    response = apply(server_mod, func_name, [req_stream, stream])
    {:ok, stream, response}
  end
  defp handle_request(true, true, %{server: server_mod} = stream, func_name, req_stream) do
    apply(server_mod, func_name, [req_stream, stream])
    {:ok, stream}
  end

  @doc """
  Start the gRPC server.

  A generated `port` will be returned if the port is `0`.

  ## Examples

      iex> {:ok, _, port} = GRPC.Server.start(Greeter.Server, 50051)

  ## Options

    * `:cred` - a credential created by functions of `GRPC.Credential`,
                an insecure server will be created without this option
    * `:adapter` - use a custom server adapter instead of default `GRPC.Adapter.Cowboy`
  """
  @spec start(atom, non_neg_integer, Keyword.t) :: {atom, any, non_neg_integer}
  def start(server, port, opts \\ []) do
    adapter = Keyword.get(opts, :adapter, GRPC.Adapter.Cowboy)
    adapter.start(server, port, opts)
  end

  @doc """
  Stop the server

  ## Examples

      iex> GRPC.Server.stop(Greeter.Server)

  ## Options

    * `:adapter` - use a custom adapter instead of default `GRPC.Adapter.Cowboy`
  """
  @spec stop(atom, Keyword.t) :: any
  def stop(server, opts \\ []) do
    adapter = Keyword.get(opts, :adapter, GRPC.Adapter.Cowboy)
    adapter.stop(server)
  end

  @doc """
  Send streaming reply.

  ## Examples

      iex> GRPC.Server.stream_send(stream, reply)
  """
  @spec stream_send(GRPC.Server.Stream.t, struct) :: any
  def stream_send(%{adapter: adapter, marshal: marshal} = stream, response) do
    {:ok, data} = response |> marshal.() |> GRPC.Message.to_data(iolist: true)
    adapter.stream_send(stream, data)
  end
end

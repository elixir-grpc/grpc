defmodule GRPC.Server.Test do
  @moduledoc """
  Helpers to test server interceptors without an endpoint, a server process or a socket.

  `build_stream/3` makes a `GRPC.Server.Stream` for one RPC of a server module.
  `run_interceptors/4` runs interceptors around a handler function, in the order
  and with the `init/1` call that `GRPC.Endpoint` uses. Afterwards,
  `response_headers/1`, `response_trailers/1` and `replies/1` return what the
  interceptors and the handler sent through the stream.

      test "rejects a request without a token" do
        stream = GRPC.Server.Test.build_stream(MyApp.Greeter.Server, :SayHello)
        request = %Helloworld.HelloRequest{name: "x"}

        assert {:error, %GRPC.RPCError{status: 16}} =
                 GRPC.Server.Test.run_interceptors([MyApp.AuthInterceptor], request, stream)
      end

  See the "Testing interceptors" guide.

  ## What the helper does not do

  The module does not start Cowboy, decode a request body or encode a response
  to HTTP/2 frames. It does not emit the `[:grpc, :server, :rpc]` telemetry
  events either. Code that depends on those needs an endpoint.
  """

  alias GRPC.Server.Stream

  defmodule Adapter do
    @moduledoc """
    A `GRPC.Server.Adapter` that records what the server sends, for `GRPC.Server.Test`.

    The stream payload holds the pid of an agent that stores headers, trailers
    and replies, so a handler that runs in another process still records to
    the same place.
    """
    @behaviour GRPC.Server.Adapter

    @impl true
    def start(_endpoint, _servers, _port, _opts) do
      raise ArgumentError, "#{inspect(__MODULE__)} does not start a server"
    end

    @impl true
    def stop(_endpoint, _servers) do
      raise ArgumentError, "#{inspect(__MODULE__)} does not stop a server"
    end

    @impl true
    def send_reply(%{agent: agent}, data, _opts) do
      Agent.update(agent, &Map.update!(&1, :replies, fn replies -> [data | replies] end))
    end

    @impl true
    def send_headers(%{agent: agent}, headers) do
      Agent.update(
        agent,
        &Map.update!(&1, :sent_headers, fn sent -> Map.merge(sent, headers) end)
      )
    end

    def set_headers(%{agent: agent}, headers) do
      Agent.update(
        agent,
        &Map.update!(&1, :headers, fn set -> Map.merge(set, Map.new(headers)) end)
      )
    end

    def set_resp_trailers(%{agent: agent}, trailers) do
      Agent.update(
        agent,
        &Map.update!(&1, :trailers, fn set -> Map.merge(set, Map.new(trailers)) end)
      )
    end

    def send_trailers(payload, trailers), do: set_resp_trailers(payload, trailers)

    def set_compressor(%{agent: agent}, compressor) do
      Agent.update(agent, &Map.put(&1, :compressor, compressor))
    end

    def get_headers(%{request_headers: headers}), do: headers

    def get_peer(%{peer: peer}), do: peer

    def get_cert(%{cert: cert}), do: cert
  end

  @doc """
  Builds a stream for the RPC `rpc_name` of `server`, with no socket behind it.

  `server` is a module that uses `GRPC.Server`. `rpc_name` is the name in the
  service definition, such as `:SayHello`, or its underscored form, `:say_hello`.
  The stream has the same `:rpc`, `:service_name`, `:method_name`, `:grpc_type`,
  `:request_mod` and `:response_mod` that the server sets for a real call.

  ## Options

    * `:headers` - request headers, as a map or a list of `{name, value}`.
      Names are downcased, as on the wire. Defaults to `%{}`.
    * `:timeout` - the request deadline in milliseconds from now. Without it,
      the deadline comes from a `"grpc-timeout"` header, if there is one.
    * `:local` - the stream's `:local` data, as in the `:local` option of an endpoint.
    * `:endpoint` - the endpoint module to put in the stream. Defaults to `nil`.
    * `:peer` - what `get_peer` returns. Defaults to `{{127, 0, 0, 1}, 50_000}`.
    * `:cert` - what `get_cert` returns. Defaults to `:undefined`.
    * `:codec`, `:compressor`, `:http_method`, `:request_id` - set the stream
      field of the same name.

  The stream records to an agent linked to the calling process.
  """
  @spec build_stream(module(), atom() | String.t(), keyword()) :: Stream.t()
  def build_stream(server, rpc_name, opts \\ []) do
    {service_mod, rpc} = fetch_rpc!(server, rpc_name)
    {name, {req_mod, _}, {res_mod, _}, _} = rpc

    headers = normalize_headers(Keyword.get(opts, :headers, %{}))

    {:ok, agent} =
      Agent.start_link(fn ->
        %{headers: %{}, sent_headers: %{}, trailers: %{}, replies: [], compressor: nil}
      end)

    payload = %{
      agent: agent,
      request_headers: headers,
      peer: Keyword.get(opts, :peer, {{127, 0, 0, 1}, 50_000}),
      cert: Keyword.get(opts, :cert, :undefined)
    }

    %Stream{
      server: server,
      endpoint: Keyword.get(opts, :endpoint),
      service_name: service_mod.__meta__(:name),
      method_name: to_string(name),
      grpc_type: GRPC.Service.grpc_type(rpc),
      rpc: rpc,
      request_mod: req_mod,
      response_mod: res_mod,
      request_id: Keyword.get_lazy(opts, :request_id, &request_id/0),
      codec: Keyword.get(opts, :codec, GRPC.Codec.Proto),
      compressor: Keyword.get(opts, :compressor),
      http_method: Keyword.get(opts, :http_method, :post),
      http_request_headers: headers,
      local: Keyword.get(opts, :local),
      deadline: deadline(opts, headers),
      adapter: Adapter,
      payload: payload
    }
  end

  @doc """
  Runs `interceptors` around `handler_fun`, the way the server does.

  `interceptors` is a list of `module` or `{module, opts}`. Each module's
  `init/1` is called with `opts` (`[]` for a bare module), as `GRPC.Endpoint`
  does when it compiles. The first one in the list runs first, so its `call/4`
  is the outermost.

  `handler_fun` takes the request and the stream and returns the reply, like
  an RPC function of a `GRPC.Server`. For an RPC that streams its response,
  the chain returns `{:ok, stream}`; for any other, `{:ok, stream, reply}`.
  Without `handler_fun`, the reply is `nil`.

  Returns what the chain returns. If an interceptor or the handler raises a
  `GRPC.RPCError`, returns `{:error, error}`, which is what the server's
  telemetry span does with it. Any other exception propagates to the test.
  """
  @spec run_interceptors(
          [module() | {module(), any()}],
          struct() | nil,
          Stream.t(),
          (struct() | nil, Stream.t() -> any())
        ) :: GRPC.Server.rpc_return() | {:error, GRPC.RPCError.t()}
  def run_interceptors(interceptors, req, %Stream{} = stream, handler_fun \\ fn _, _ -> nil end)
      when is_list(interceptors) and is_function(handler_fun, 2) do
    last = fn r, s ->
      reply = handler_fun.(r, s)

      case stream.rpc do
        {_, _, {_, true}, _} -> {:ok, stream}
        _ -> {:ok, stream, reply}
      end
    end

    chain =
      interceptors
      |> Enum.map(&init_interceptor/1)
      |> GRPC.Server.build_chain(last)

    chain.(req, stream)
  rescue
    e in GRPC.RPCError -> {:error, e}
  end

  @doc """
  The response headers set with `GRPC.Server.set_headers/2` or sent with
  `GRPC.Server.send_headers/2`.
  """
  @spec response_headers(Stream.t()) :: map()
  def response_headers(%Stream{payload: %{agent: agent}}) do
    %{headers: headers, sent_headers: sent} = Agent.get(agent, & &1)
    Map.merge(headers, sent)
  end

  @doc "The trailers set with `GRPC.Server.set_trailers/2`."
  @spec response_trailers(Stream.t()) :: map()
  def response_trailers(%Stream{payload: %{agent: agent}}) do
    Agent.get(agent, & &1.trailers)
  end

  @doc """
  The replies sent with `GRPC.Server.send_reply/3`, oldest first, as the
  adapter would get them: encoded with the stream's codec.
  """
  @spec replies(Stream.t()) :: [iodata()]
  def replies(%Stream{payload: %{agent: agent}}) do
    agent |> Agent.get(& &1.replies) |> Enum.reverse()
  end

  defp init_interceptor({interceptor, opts}), do: {interceptor, interceptor.init(opts)}
  defp init_interceptor(interceptor), do: {interceptor, interceptor.init([])}

  defp fetch_rpc!(server, rpc_name) do
    service_mod = server.__meta__(:service)
    wanted = to_string(rpc_name)

    rpc =
      Enum.find(service_mod.__rpc_calls__(), fn {name, _, _, _} ->
        name = to_string(name)
        name == wanted or Macro.underscore(name) == wanted
      end)

    if rpc do
      {service_mod, rpc}
    else
      raise ArgumentError,
            "#{inspect(server)} has no rpc #{inspect(rpc_name)}. " <>
              "Its rpcs are: #{Enum.map_join(service_mod.__rpc_calls__(), ", ", &inspect(elem(&1, 0)))}"
    end
  end

  defp normalize_headers(headers) do
    Map.new(headers, fn {name, value} -> {name |> to_string() |> String.downcase(), value} end)
  end

  defp deadline(opts, headers) do
    timeout =
      case Keyword.fetch(opts, :timeout) do
        {:ok, timeout} ->
          timeout

        :error ->
          case headers do
            %{"grpc-timeout" => value} when is_binary(value) ->
              GRPC.Transport.Utils.decode_timeout(value)

            _ ->
              nil
          end
      end

    timeout && System.monotonic_time(:millisecond) + timeout
  end

  defp request_id do
    Base.url_encode64(:crypto.strong_rand_bytes(12), padding: false)
  end
end

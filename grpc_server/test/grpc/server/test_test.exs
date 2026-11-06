defmodule GRPC.Server.TestTest do
  use GRPC.Integration.TestCase, async: true

  alias GRPC.Server.Test, as: ServerTest

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule RouteServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def list_features(_rect, _stream), do: nil
  end

  # Rejects a request without an `authorization` header.
  defmodule AuthInterceptor do
    @behaviour GRPC.Server.Interceptor

    @impl true
    def init(opts), do: opts

    @impl true
    def call(req, stream, next, _opts) do
      if Map.has_key?(stream.http_request_headers, "authorization") do
        next.(req, stream)
      else
        raise GRPC.RPCError, status: :unauthenticated, message: "missing authorization"
      end
    end
  end

  defmodule HeaderInterceptor do
    @behaviour GRPC.Server.Interceptor

    @impl true
    def init(opts), do: Keyword.validate!(opts, header: {"x-served-by", "test"})

    @impl true
    def call(req, stream, next, opts) do
      {name, value} = Keyword.fetch!(opts, :header)
      stream = GRPC.Server.set_headers(stream, %{name => value})
      next.(req, stream)
    end
  end

  # Sends the time the rest of the chain took to the process in `:notify`.
  defmodule TimingInterceptor do
    @behaviour GRPC.Server.Interceptor

    @impl true
    def init(opts), do: Keyword.fetch!(opts, :notify)

    @impl true
    def call(req, stream, next, notify) do
      started = System.monotonic_time()
      result = next.(req, stream)
      send(notify, {:elapsed, System.monotonic_time() - started})
      result
    end
  end

  # Tells the test when it enters and leaves, to show the order.
  defmodule OrderInterceptor do
    @behaviour GRPC.Server.Interceptor

    @impl true
    def init(opts), do: Keyword.fetch!(opts, :name)

    @impl true
    def call(req, stream, next, name) do
      send(self(), {:enter, name})
      result = next.(req, stream)
      send(self(), {:leave, name})
      result
    end
  end

  defmodule Endpoint do
    use GRPC.Endpoint

    intercept AuthInterceptor
    intercept HeaderInterceptor

    run HelloServer
  end

  @request %Helloworld.HelloRequest{name: "world"}

  defp hello_stream(opts \\ []), do: ServerTest.build_stream(HelloServer, :SayHello, opts)

  describe "build_stream/3" do
    test "fills the fields the server sets for a real call" do
      stream = hello_stream(headers: [{"Authorization", "token"}], local: :data)

      assert %GRPC.Server.Stream{
               server: HelloServer,
               endpoint: nil,
               service_name: "helloworld.Greeter",
               method_name: "SayHello",
               grpc_type: :unary,
               request_mod: Helloworld.HelloRequest,
               response_mod: Helloworld.HelloReply,
               http_request_headers: %{"authorization" => "token"},
               local: :data,
               deadline: nil,
               adapter: ServerTest.Adapter
             } = stream

      assert elem(stream.rpc, 0) == :SayHello
      assert is_binary(stream.request_id)
    end

    test "takes the rpc name in underscored form" do
      assert hello_stream().rpc == ServerTest.build_stream(HelloServer, :say_hello).rpc
    end

    test "raises for an rpc the server does not have" do
      assert_raise ArgumentError, ~r/no rpc :Nope/, fn ->
        ServerTest.build_stream(HelloServer, :Nope)
      end
    end

    test "sets the deadline from :timeout or from the grpc-timeout header" do
      now = System.monotonic_time(:millisecond)

      assert GRPC.Server.Stream.remaining_ms(hello_stream(timeout: 5_000)) in 4_000..5_000
      assert hello_stream(headers: %{"grpc-timeout" => "2S"}).deadline >= now + 2_000
      assert GRPC.Server.Stream.remaining_ms(hello_stream()) == :infinity
    end

    test "answers get_headers, get_peer and get_cert" do
      stream = hello_stream(headers: %{"x-a" => "b"}, peer: {{10, 0, 0, 1}, 1234})

      assert GRPC.Stream.get_headers(stream) == %{"x-a" => "b"}
      assert stream.adapter.get_peer(stream.payload) == {{10, 0, 0, 1}, 1234}
      assert stream.adapter.get_cert(stream.payload) == :undefined
    end

    test "marks a server-streaming rpc" do
      stream = ServerTest.build_stream(RouteServer, :ListFeatures)
      assert stream.grpc_type == :server_stream
    end
  end

  describe "run_interceptors/4" do
    test "an interceptor that rejects a request without an auth header" do
      assert {:error, %GRPC.RPCError{status: 16, message: "missing authorization"}} =
               ServerTest.run_interceptors([AuthInterceptor], @request, hello_stream())

      stream = hello_stream(headers: %{"authorization" => "token"})
      handler = fn req, _stream -> %Helloworld.HelloReply{message: req.name} end

      assert {:ok, _stream, %Helloworld.HelloReply{message: "world"}} =
               ServerTest.run_interceptors([AuthInterceptor], @request, stream, handler)
    end

    test "the handler does not run when an interceptor rejects the request" do
      test_pid = self()

      handler = fn _req, _stream ->
        send(test_pid, :handler_ran)
        nil
      end

      assert {:error, _} =
               ServerTest.run_interceptors([AuthInterceptor], @request, hello_stream(), handler)

      refute_received :handler_ran
    end

    test "an interceptor that adds response headers" do
      stream = hello_stream()

      assert {:ok, _, nil} =
               ServerTest.run_interceptors(
                 [{HeaderInterceptor, header: {"x-request-id", "abc"}}],
                 @request,
                 stream
               )

      assert ServerTest.response_headers(stream) == %{"x-request-id" => "abc"}
    end

    test "calls init/1 with [] for a bare module" do
      stream = hello_stream()
      assert {:ok, _, nil} = ServerTest.run_interceptors([HeaderInterceptor], @request, stream)
      assert ServerTest.response_headers(stream) == %{"x-served-by" => "test"}
    end

    test "an interceptor that measures time" do
      handler = fn _req, _stream ->
        Process.sleep(10)
        nil
      end

      ServerTest.run_interceptors(
        [{TimingInterceptor, notify: self()}],
        @request,
        hello_stream(),
        handler
      )

      assert_receive {:elapsed, elapsed}
      assert System.convert_time_unit(elapsed, :native, :millisecond) >= 10
    end

    test "the first interceptor in the list is the outermost" do
      ServerTest.run_interceptors(
        [{OrderInterceptor, name: :a}, {OrderInterceptor, name: :b}],
        @request,
        hello_stream()
      )

      assert {:messages, [{:enter, :a}, {:enter, :b}, {:leave, :b}, {:leave, :a}]} =
               Process.info(self(), :messages)
    end

    test "returns {:ok, stream} when the rpc streams its response" do
      stream = ServerTest.build_stream(RouteServer, :ListFeatures)

      assert {:ok, %GRPC.Server.Stream{}} =
               ServerTest.run_interceptors([], %Routeguide.Rectangle{}, stream, fn _, _ -> nil end)
    end

    test "records replies, headers and trailers sent by the handler" do
      stream = hello_stream()

      handler = fn _req, stream ->
        GRPC.Server.send_headers(stream, %{"x-sent" => "1"})
        GRPC.Server.set_trailers(stream, %{"x-trailer" => "2"})
        GRPC.Server.send_reply(stream, %Helloworld.HelloReply{message: "hi"})
        nil
      end

      ServerTest.run_interceptors([], @request, stream, handler)

      assert ServerTest.response_headers(stream) == %{"x-sent" => "1"}
      assert ServerTest.response_trailers(stream) == %{"x-trailer" => "2"}
      assert [reply] = ServerTest.replies(stream)

      assert Protobuf.decode(IO.iodata_to_binary(reply), Helloworld.HelloReply).message == "hi"
    end

    test "lets an exception other than GRPC.RPCError propagate" do
      defmodule Crash do
        def init(_), do: nil
        def call(_req, _stream, _next, _opts), do: raise("boom")
      end

      assert_raise RuntimeError, "boom", fn ->
        ServerTest.run_interceptors([Crash], @request, hello_stream())
      end
    end
  end

  describe "against a real endpoint" do
    # Calls the endpoint over HTTP/2 and returns the grpc-status and the
    # x-served-by response header.
    defp call_endpoint(port, headers) do
      {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http2]})
      {:ok, :http2} = :gun.await_up(conn, 5_000)

      body = Protobuf.encode(@request)
      frame = <<0::8, byte_size(body)::32, body::binary>>

      ref =
        :gun.post(
          conn,
          "/helloworld.Greeter/SayHello",
          [{"content-type", "application/grpc+proto"}, {"te", "trailers"}] ++ headers,
          frame
        )

      result = collect(conn, ref, %{})
      :gun.close(conn)
      result
    end

    defp collect(conn, ref, acc) do
      case :gun.await(conn, ref, 5_000) do
        {:response, fin, _status, headers} ->
          acc = merge_headers(acc, headers)
          if fin == :fin, do: summarize(acc), else: collect(conn, ref, acc)

        {:data, :fin, _} ->
          summarize(acc)

        {:data, :nofin, _} ->
          collect(conn, ref, acc)

        {:trailers, trailers} ->
          summarize(merge_headers(acc, trailers))
      end
    end

    defp merge_headers(acc, headers), do: Enum.into(headers, acc)

    defp summarize(headers), do: {headers["grpc-status"], headers["x-served-by"]}

    # The same outcome, read from what the helper returned.
    defp summarize_helper({:ok, stream, _reply}) do
      {"0", ServerTest.response_headers(stream)["x-served-by"]}
    end

    defp summarize_helper({:error, %GRPC.RPCError{status: status}}) do
      {Integer.to_string(status), nil}
    end

    for {name, headers} <- [
          {"without an auth header", []},
          {"with an auth header", [{"authorization", "token"}]}
        ] do
      test "the helper and the endpoint agree #{name}" do
        headers = unquote(Macro.escape(headers))

        via_helper =
          [AuthInterceptor, HeaderInterceptor]
          |> ServerTest.run_interceptors(@request, hello_stream(headers: headers))
          |> summarize_helper()

        via_endpoint = run_endpoint(Endpoint, fn port -> call_endpoint(port, headers) end)

        assert via_helper == via_endpoint
      end
    end

    test "the two cases give different results" do
      assert {"16", nil} ==
               run_endpoint(Endpoint, fn port -> call_endpoint(port, []) end)

      assert {"0", "test"} ==
               run_endpoint(Endpoint, fn port ->
                 call_endpoint(port, [{"authorization", "token"}])
               end)
    end
  end
end

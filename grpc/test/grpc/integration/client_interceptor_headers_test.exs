defmodule GRPC.Integration.ClientInterceptorHeadersTest do
  use GRPC.Integration.TestCase

  alias Routeguide.{Feature, Point, Rectangle, RouteNote, RouteSummary}
  alias Routeguide.RouteGuide.Stub

  defmodule Server do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def get_feature(point, stream) do
      reply_metadata(stream)
      %Feature{location: point, name: "one"}
    end

    def list_features(_rect, stream) do
      reply_metadata(stream)

      for name <- ["one", "two"] do
        GRPC.Server.send_reply(stream, %Feature{name: name})
      end

      GRPC.Server.set_trailers(stream, %{"x-test-trailer" => "from-server"})
    end

    def record_route(req_enum, stream) do
      count = req_enum |> Enum.to_list() |> length()
      reply_metadata(stream)
      %RouteSummary{point_count: count}
    end

    def route_chat(req_enum, stream) do
      GRPC.Server.set_headers(stream, %{"x-test-header" => "from-server"})

      Enum.each(req_enum, fn note -> GRPC.Server.send_reply(stream, note) end)

      GRPC.Server.set_trailers(stream, %{"x-test-trailer" => "from-server"})
    end

    defp reply_metadata(stream) do
      GRPC.Server.set_headers(stream, %{"x-test-header" => "from-server"})
      GRPC.Server.set_trailers(stream, %{"x-test-trailer" => "from-server"})
    end
  end

  # Reports what it sees to the test process and returns the result unchanged.
  # `call/4` covers unary and server-streaming RPCs, `handle_recv/3` covers
  # client-streaming and bidi RPCs, which `call/4` only sees opening.
  # It opts in to headers unless its options say `return_headers: false`.
  defmodule Recorder do
    @behaviour GRPC.Client.Interceptor

    def init(opts), do: opts

    def return_headers?(opts), do: Map.get(opts, :return_headers, true)

    def call(stream, req, next, opts) do
      stream |> next.(req) |> observe(opts)
    end

    def handle_recv(_stream, result, opts), do: observe(result, opts)

    defp observe({:ok, _reply, %{headers: headers, trailers: trailers}} = result, %{
           pid: pid,
           name: name
         }) do
      send(pid, {name, :headers, headers})
      send(pid, {name, :trailers, trailers})
      result
    end

    # A streaming reply: trailers arrive as the last item, so watch for it.
    defp observe({:ok, enum, %{headers: headers} = meta}, %{pid: pid, name: name}) do
      send(pid, {name, :headers, headers})

      watched =
        Stream.map(enum, fn
          {:trailers, trailers} = item ->
            send(pid, {name, :trailers, trailers})
            item

          item ->
            item
        end)

      {:ok, watched, meta}
    end

    defp observe(result, %{pid: pid, name: name}) do
      send(pid, {name, :no_headers, result})
      result
    end
  end

  # Only implements call/4, like every interceptor written before handle_recv/3.
  defmodule Plain do
    @behaviour GRPC.Client.Interceptor

    def init(opts), do: opts
    def call(stream, req, next, _opts), do: next.(stream, req)
  end

  # No `return_headers?/1`. Matches `{:ok, reply}` and `{:ok, replies}` on the
  # result of `next`, as interceptors written before the opt-in do.
  defmodule Matcher do
    @behaviour GRPC.Client.Interceptor

    def init(opts), do: opts

    def call(%{server_stream: true} = stream, req, next, %{pid: pid, name: name}) do
      case next.(stream, req) do
        {:ok, replies} ->
          {:ok, Stream.each(replies, &send(pid, {name, :item, &1}))}

        other ->
          send(pid, {name, :other, other})
          other
      end
    end

    def call(stream, req, next, %{pid: pid, name: name}) do
      case next.(stream, req) do
        {:ok, reply} ->
          send(pid, {name, :short, reply})
          {:ok, reply}

        other ->
          send(pid, {name, :other, other})
          other
      end
    end
  end

  # No `return_headers?/1`. Changes every reply of a server stream.
  defmodule Upcaser do
    @behaviour GRPC.Client.Interceptor

    def init(opts), do: opts

    def call(stream, req, next, _opts) do
      case next.(stream, req) do
        {:ok, replies} ->
          {:ok,
           Stream.map(replies, fn {:ok, %Feature{name: name} = feature} ->
             {:ok, %{feature | name: String.upcase(name)}}
           end)}

        other ->
          other
      end
    end
  end

  defmodule Endpoint do
    use GRPC.Endpoint

    run(Server)
  end

  @adapters [GRPC.Client.Adapters.Gun, GRPC.Client.Adapters.Mint]

  # Wraps an adapter and sends the opts of `send_request/3` to the calling process.
  for adapter <- @adapters do
    name = Module.concat(__MODULE__, "Recording" <> (adapter |> Module.split() |> List.last()))

    Module.create(
      name,
      quote do
        @behaviour GRPC.Client.Adapter
        @adapter unquote(adapter)

        def connect(channel, opts), do: @adapter.connect(channel, opts)
        def disconnect(channel), do: @adapter.disconnect(channel)
        def receive_data(stream, opts), do: @adapter.receive_data(stream, opts)
        def send_headers(stream, opts), do: @adapter.send_headers(stream, opts)
        def send_data(stream, message, opts), do: @adapter.send_data(stream, message, opts)
        def end_stream(stream), do: @adapter.end_stream(stream)
        def cancel(stream), do: @adapter.cancel(stream)

        def send_request(stream, message, opts) do
          send(self(), {:adapter_opts, opts})
          @adapter.send_request(stream, message, opts)
        end
      end,
      Macro.Env.location(__ENV__)
    )
  end

  for adapter <- @adapters do
    describe "with #{inspect(adapter)}" do
      @describetag adapter: adapter

      test "unary: the interceptor reads headers and trailers, the caller keeps its result",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})

          assert_received {:one, :headers, headers}
          assert headers["x-test-header"] == "from-server"
          assert_received {:one, :trailers, trailers}
          assert trailers["x-test-trailer"] == "from-server"
        end)
      end

      test "unary with return_headers: the caller still gets headers and trailers",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          assert {:ok, %Feature{name: "one"}, %{headers: headers, trailers: trailers}} =
                   Stub.get_feature(channel, %Point{}, return_headers: true)

          assert headers["x-test-header"] == "from-server"
          assert trailers["x-test-trailer"] == "from-server"

          assert_received {:one, :headers, ^headers}
          assert_received {:one, :trailers, ^trailers}
        end)
      end

      test "server streaming: the interceptor reads headers and trailers, the caller keeps its result",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "one"}}, {:ok, %Feature{name: "two"}}] =
                   Enum.to_list(replies)

          assert_received {:one, :headers, headers}
          assert headers["x-test-header"] == "from-server"
          assert_received {:one, :trailers, trailers}
          assert trailers["x-test-trailer"] == "from-server"
        end)
      end

      test "server streaming with return_headers: the caller still gets the trailers item",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          assert {:ok, replies, %{headers: headers}} =
                   Stub.list_features(channel, %Rectangle{}, return_headers: true)

          assert headers["x-test-header"] == "from-server"

          assert [
                   {:ok, %Feature{name: "one"}},
                   {:ok, %Feature{name: "two"}},
                   {:trailers, %{"x-test-trailer" => "from-server"}}
                 ] = Enum.to_list(replies)

          assert_received {:one, :headers, ^headers}
          assert_received {:one, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "an interceptor that returns false from return_headers?/1 gets the short shape",
           %{adapter: adapter} do
        interceptor = {Recorder, %{pid: self(), name: :one, return_headers: false}}

        with_channel(adapter, [interceptor], fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})
          assert_received {:one, :no_headers, {:ok, %Feature{name: "one"}}}
        end)
      end

      test "unary: an interceptor without the opt-in matches {:ok, reply}", %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain)], fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})
          assert_received {:plain, :short, %Feature{name: "one"}}
        end)
      end

      test "unary with return_headers: an interceptor without the opt-in gets the long shape",
           %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain)], fn channel ->
          assert {:ok, %Feature{}, %{headers: _, trailers: _}} =
                   Stub.get_feature(channel, %Point{}, return_headers: true)

          assert_received {:plain, :other, {:ok, %Feature{}, %{headers: _, trailers: _}}}
        end)
      end

      test "server streaming: an interceptor without the opt-in matches {:ok, replies}",
           %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain)], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "one"}}, {:ok, %Feature{name: "two"}}] =
                   Enum.to_list(replies)

          assert_received {:plain, :item, {:ok, %Feature{name: "one"}}}
          assert_received {:plain, :item, {:ok, %Feature{name: "two"}}}
          refute_received {:plain, :item, {:trailers, _}}
        end)
      end

      test "server streaming with return_headers: an interceptor without the opt-in gets the long shape",
           %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain)], fn channel ->
          assert {:ok, replies, %{headers: _}} =
                   Stub.list_features(channel, %Rectangle{}, return_headers: true)

          assert [_, _, {:trailers, _}] = Enum.to_list(replies)
          assert_received {:plain, :other, {:ok, _replies, %{headers: _}}}
        end)
      end

      test "unary: an opted-in interceptor outside a plain one still gets the headers",
           %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain), recorder(:out)], fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})

          assert_received {:plain, :short, %Feature{name: "one"}}
          assert_received {:out, :headers, %{"x-test-header" => "from-server"}}
          assert_received {:out, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "unary: a plain interceptor outside an opted-in one gets the short shape",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:in), matcher(:plain)], fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})

          assert_received {:in, :headers, %{"x-test-header" => "from-server"}}
          assert_received {:in, :trailers, %{"x-test-trailer" => "from-server"}}
          assert_received {:plain, :short, %Feature{name: "one"}}
        end)
      end

      test "server streaming: an opted-in interceptor outside a plain one still gets the trailers",
           %{adapter: adapter} do
        with_channel(adapter, [matcher(:plain), recorder(:out)], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "one"}}, {:ok, %Feature{name: "two"}}] =
                   Enum.to_list(replies)

          assert_received {:plain, :item, {:ok, %Feature{name: "two"}}}
          refute_received {:plain, :item, {:trailers, _}}
          assert_received {:out, :headers, %{"x-test-header" => "from-server"}}
          assert_received {:out, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "server streaming: a plain interceptor outside an opted-in one gets no trailers item",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:in), matcher(:plain)], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "one"}}, {:ok, %Feature{name: "two"}}] =
                   Enum.to_list(replies)

          assert_received {:in, :headers, %{"x-test-header" => "from-server"}}
          assert_received {:in, :trailers, %{"x-test-trailer" => "from-server"}}
          assert_received {:plain, :item, {:ok, %Feature{name: "two"}}}
          refute_received {:plain, :item, {:trailers, _}}
        end)
      end

      test "server streaming: plain interceptors between opted-in ones pass the trailers on",
           %{adapter: adapter} do
        interceptors = [
          recorder(:inner),
          matcher(:p1),
          {Upcaser, []},
          matcher(:p2),
          recorder(:outer)
        ]

        with_channel(adapter, interceptors, fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "ONE"}}, {:ok, %Feature{name: "TWO"}}] =
                   Enum.to_list(replies)

          for name <- [:inner, :outer] do
            assert_received {^name, :headers, %{"x-test-header" => "from-server"}}
            assert_received {^name, :trailers, %{"x-test-trailer" => "from-server"}}
          end
        end)
      end

      test "server streaming: the changes of a plain interceptor reach the caller and the outer interceptor",
           %{adapter: adapter} do
        with_channel(adapter, [{Upcaser, []}, recorder(:out)], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "ONE"}}, {:ok, %Feature{name: "TWO"}}] =
                   Enum.to_list(replies)

          assert_received {:out, :headers, %{"x-test-header" => "from-server"}}
          assert_received {:out, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "server streaming: the changes of a plain interceptor reach the caller when it is outside",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:in), {Upcaser, []}], fn channel ->
          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})

          assert [{:ok, %Feature{name: "ONE"}}, {:ok, %Feature{name: "TWO"}}] =
                   Enum.to_list(replies)

          assert_received {:in, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "the adapter is not asked for headers without an opt-in or a caller flag",
           %{adapter: adapter} do
        with_recording_channel(adapter, [matcher(:plain), {Plain, []}], fn channel ->
          assert {:ok, %Feature{}} = Stub.get_feature(channel, %Point{})
          assert_received {:adapter_opts, opts}
          refute opts[:return_headers]

          assert {:ok, replies} = Stub.list_features(channel, %Rectangle{})
          assert [_, _] = Enum.to_list(replies)
          assert_received {:adapter_opts, opts}
          refute opts[:return_headers]
        end)
      end

      test "the adapter is asked for headers when an interceptor opts in or the caller asks",
           %{adapter: adapter} do
        with_recording_channel(adapter, [matcher(:plain), recorder(:one)], fn channel ->
          assert {:ok, %Feature{}} = Stub.get_feature(channel, %Point{})
          assert_received {:adapter_opts, opts}
          assert opts[:return_headers]
        end)

        with_recording_channel(adapter, [matcher(:plain)], fn channel ->
          assert {:ok, %Feature{}, _} = Stub.get_feature(channel, %Point{}, return_headers: true)
          assert_received {:adapter_opts, opts}
          assert opts[:return_headers]
        end)
      end

      test "client streaming: the interceptor reads headers and trailers, the caller keeps its result",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          stream = Stub.record_route(channel)
          stream = GRPC.Stub.send_request(stream, %Point{}, end_stream: true)

          assert {:ok, %RouteSummary{point_count: 1}} = GRPC.Stub.recv(stream)

          assert_received {:one, :headers, headers}
          assert headers["x-test-header"] == "from-server"
          assert_received {:one, :trailers, trailers}
          assert trailers["x-test-trailer"] == "from-server"
        end)
      end

      test "client streaming with return_headers: the caller still gets headers and trailers",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          stream = Stub.record_route(channel, return_headers: true)
          stream = GRPC.Stub.send_request(stream, %Point{}, end_stream: true)

          assert {:ok, %RouteSummary{point_count: 1}, %{headers: headers, trailers: trailers}} =
                   GRPC.Stub.recv(stream, return_headers: true)

          assert headers["x-test-header"] == "from-server"
          assert trailers["x-test-trailer"] == "from-server"

          assert_received {:one, :headers, ^headers}
          assert_received {:one, :trailers, ^trailers}
        end)
      end

      test "bidi: the interceptor reads headers and trailers, the caller keeps its result",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          stream = Stub.route_chat(channel)
          stream = GRPC.Stub.send_request(stream, %RouteNote{message: "hi"}, end_stream: true)

          assert {:ok, replies} = GRPC.Stub.recv(stream)
          assert [{:ok, %RouteNote{message: "hi"}}] = Enum.to_list(replies)

          assert_received {:one, :headers, headers}
          assert headers["x-test-header"] == "from-server"
          assert_received {:one, :trailers, trailers}
          assert trailers["x-test-trailer"] == "from-server"
        end)
      end

      test "bidi with return_headers: the caller still gets the trailers item",
           %{adapter: adapter} do
        with_channel(adapter, [recorder(:one)], fn channel ->
          stream = Stub.route_chat(channel, return_headers: true)
          stream = GRPC.Stub.send_request(stream, %RouteNote{message: "hi"}, end_stream: true)

          assert {:ok, replies, %{headers: headers}} =
                   GRPC.Stub.recv(stream, return_headers: true)

          assert headers["x-test-header"] == "from-server"

          assert [
                   {:ok, %RouteNote{message: "hi"}},
                   {:trailers, %{"x-test-trailer" => "from-server"}}
                 ] = Enum.to_list(replies)

          assert_received {:one, :headers, ^headers}
          assert_received {:one, :trailers, %{"x-test-trailer" => "from-server"}}
        end)
      end

      test "nested interceptors both read headers and trailers, and a plain one between them is unaffected",
           %{adapter: adapter} do
        interceptors = [recorder(:inner), {Plain, []}, recorder(:outer)]

        with_channel(adapter, interceptors, fn channel ->
          assert {:ok, %Feature{name: "one"}} = Stub.get_feature(channel, %Point{})

          for name <- [:inner, :outer] do
            assert_received {^name, :headers, %{"x-test-header" => "from-server"}}
            assert_received {^name, :trailers, %{"x-test-trailer" => "from-server"}}
          end

          stream = Stub.record_route(channel)
          stream = GRPC.Stub.send_request(stream, %Point{}, end_stream: true)
          assert {:ok, %RouteSummary{}} = GRPC.Stub.recv(stream)

          for name <- [:inner, :outer] do
            assert_received {^name, :headers, %{"x-test-header" => "from-server"}}
            assert_received {^name, :trailers, %{"x-test-trailer" => "from-server"}}
          end
        end)
      end

      test "an interceptor without handle_recv/3 leaves streaming calls as they were",
           %{adapter: adapter} do
        with_channel(adapter, [{Plain, []}], fn channel ->
          stream = Stub.route_chat(channel)
          stream = GRPC.Stub.send_request(stream, %RouteNote{message: "hi"}, end_stream: true)

          assert {:ok, replies} = GRPC.Stub.recv(stream)
          assert [{:ok, %RouteNote{message: "hi"}}] = Enum.to_list(replies)
        end)
      end
    end
  end

  defp recorder(name), do: {Recorder, %{pid: self(), name: name}}
  defp matcher(name), do: {Matcher, %{pid: self(), name: name}}

  defp with_recording_channel(adapter, interceptors, fun) do
    name = Module.concat(__MODULE__, "Recording" <> (adapter |> Module.split() |> List.last()))
    with_channel(name, interceptors, fun)
  end

  defp with_channel(adapter, interceptors, fun) do
    run_endpoint(Endpoint, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", adapter: adapter, interceptors: interceptors)

      try do
        fun.(channel)
      after
        GRPC.Stub.disconnect(channel)
      end
    end)
  end
end

defmodule GRPC.Integration.ClientInterceptorTest do
  use GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, stream) do
      headers = GRPC.Stream.get_headers(stream)
      label = headers["x-test-label"]
      Helloworld.HelloReply.new(message: "Hello, #{req.name} #{label}")
    end
  end

  defmodule AddHeadersClientInterceptor do
    @behaviour GRPC.Client.Interceptor

    def init(label), do: label

    def call(%{headers: headers} = stream, req, next, label) do
      new_headers =
        case Map.get(headers, "x-test-label") do
          nil -> %{"x-test-label" => label}
          original -> %{headers | "x-test-label" => "#{original} #{label}"}
        end

      new_stream = GRPC.Client.Stream.put_headers(stream, new_headers)
      next.(new_stream, req)
    end
  end

  defmodule RaiseClientInterceptor do
    @behaviour GRPC.Client.Interceptor

    def init(opts), do: opts

    def call(_stream, _req, _next, %{
          error_function: error_function,
          delay: delay,
          message: message
        }) do
      Process.sleep(delay)
      error_function.(message)
    end
  end

  defmodule HelloEndpoint do
    use GRPC.Endpoint

    run(HelloServer)
  end

  test "client sends headers" do
    run_endpoint(HelloEndpoint, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}",
          interceptors: [
            {AddHeadersClientInterceptor, "two"},
            {AddHeadersClientInterceptor, "one"}
          ]
        )

      req = Helloworld.HelloRequest.new(name: "Elixir")
      {:ok, reply} = channel |> Helloworld.Greeter.Stub.say_hello(req)
      assert reply.message == "Hello, Elixir one two"
    end)
  end

  test "sends exception event upon client exception" do
    message = "exception-#{inspect(self())}"

    for {function, kind, reason} <- [
          {&throw/1, :throw, message},
          {&:erlang.exit/1, :exit, message},
          {&raise/1, :error, %RuntimeError{message: message}},
          {&:erlang.error/1, :error, %ErlangError{original: message}}
        ] do
      client_prefix = GRPC.Telemetry.client_rpc_prefix()
      stop_client_name = client_prefix ++ [:stop]
      exception_client_name = client_prefix ++ [:exception]

      attach_events([
        stop_client_name,
        exception_client_name
      ])

      run_endpoint(HelloEndpoint, fn port ->
        delay = floor(:rand.uniform() * 500) + 500

        {:ok, channel} =
          GRPC.Stub.connect("localhost:#{port}",
            interceptors: [
              {RaiseClientInterceptor,
               %{error_function: function, message: message, delay: delay}}
            ]
          )

        req = Helloworld.HelloRequest.new(name: "Elixir")

        try do
          Helloworld.Greeter.Stub.say_hello(channel, req)
        rescue
          _ -> :ok
        catch
          _, _ -> :ok
        else
          _ -> flunk("did not raise")
        end

        assert_received {^exception_client_name, measurements, metadata}
        assert %{duration: duration} = measurements
        assert duration > delay

        assert %{kind: ^kind, reason: ^reason, stacktrace: stacktrace} = metadata

        assert is_list(stacktrace)

        Enum.each(stacktrace, fn entry ->
          # ensure stacktrace is a pure stacktrace
          assert {mod, fun, arity, meta} = entry
          assert is_atom(mod)
          assert is_atom(fun)
          assert is_integer(arity)
          assert is_list(meta)
        end)
      end)

      assert_receive {:gun_down, _, _, _, _}

      refute_receive _
    end
  end
end

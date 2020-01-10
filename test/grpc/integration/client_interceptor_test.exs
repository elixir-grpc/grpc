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

  defmodule HelloEndpoint do
    use GRPC.Endpoint

    run HelloServer
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
end

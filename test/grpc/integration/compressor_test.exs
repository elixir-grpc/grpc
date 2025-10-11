defmodule GRPC.Integration.CompressorTest do
  use GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server,
      service: Helloworld.Greeter.Service,
      compressors: [GRPC.Compressor.Gzip]

    def say_hello(%{name: name = "only client compress"}, stream) do
      %{"grpc-encoding" => "gzip"} = GRPC.Stream.get_headers(stream)
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end

    def say_hello(%{name: name = "only server compress"}, stream) do
      if GRPC.Stream.get_headers(stream)["grpc-encoding"] do
        raise "grpc-encoding exists!"
      end

      GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end

    def say_hello(%{name: name = "both compress"}, stream) do
      %{"grpc-encoding" => "gzip"} = GRPC.Stream.get_headers(stream)
      GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end
  end

  defmodule NoCompressServer do
    use GRPC.Server,
      service: Helloworld.Greeter.Service

    def say_hello(%{name: name}, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{name}"}
    end
  end

  defmodule AcceptCompressServer do
    use GRPC.Server,
      service: Helloworld.Greeter.Service

    def say_hello(_, stream) do
      GRPC.Server.set_compressor(stream, GRPC.Compressor.Gzip)
      %Helloworld.HelloReply{message: "Hola, Mundo! Olá, Mundo!"}
    end
  end

  defmodule HelloStub do
    use GRPC.Stub, service: Helloworld.Greeter.Service
  end

  test "only client compress" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      name = "only client compress"
      req = %Helloworld.HelloRequest{name: name}

      {:ok, reply, headers} =
        channel
        |> HelloStub.say_hello(req, compressor: GRPC.Compressor.Gzip, return_headers: true)

      assert reply.message == "Hello, #{name}"
      refute headers[:headers]["grpc-encoding"]
    end)
  end

  test "only server compress" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      name = "only server compress"
      req = %Helloworld.HelloRequest{name: name}

      # no accept-encoding header
      {:ok, reply, headers} = channel |> HelloStub.say_hello(req, return_headers: true)
      assert reply.message == "Hello, #{name}"
      refute headers[:headers]["grpc-encoding"]

      {:ok, reply, headers} =
        channel
        |> HelloStub.say_hello(req,
          return_headers: true,
          accepted_compressors: [GRPC.Compressor.Gzip]
        )

      assert reply.message == "Hello, #{name}"
      assert headers[:headers]["grpc-encoding"]
    end)
  end

  test "both sides compress" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      name = "both compress"
      req = %Helloworld.HelloRequest{name: name}

      {:ok, reply, headers} =
        channel
        |> HelloStub.say_hello(req, compressor: GRPC.Compressor.Gzip, return_headers: true)

      assert reply.message == "Hello, #{name}"
      assert headers[:headers]["grpc-encoding"]
    end)
  end

  test "error when server doesn't support" do
    run_server(NoCompressServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      name = "both compress"
      req = %Helloworld.HelloRequest{name: name}

      assert {:error, %GRPC.RPCError{message: _, status: 12}} =
               channel
               |> HelloStub.say_hello(req, compressor: GRPC.Compressor.Gzip, return_headers: true)
    end)
  end

  test "error when accepted_compressors is not a list" do
    run_server(AcceptCompressServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      assert_raise ArgumentError, "accepted_compressors is not a list", fn ->
        HelloStub.say_hello(channel, %Helloworld.HelloRequest{name: ""},
          accepted_compressors: nil
        )
      end
    end)
  end

  test "fallback to channel's accepted_compressors" do
    run_server(AcceptCompressServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", accepted_compressors: [GRPC.Compressor.Gzip])

      req = %Helloworld.HelloRequest{name: ""}

      {:ok, _reply, headers} = HelloStub.say_hello(channel, req, return_headers: true)
      assert headers[:headers]["grpc-encoding"]
    end)
  end
end

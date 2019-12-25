defmodule GRPC.Integration.ErplackNotypesTest do
  use GRPC.Integration.TestCase

  defmodule Helloworld.Notypes.Service do
    use GRPC.Service, name: "helloworld.Notypes"

    rpc :ReplyHello, :ignore, :ignore
  end

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Notypes.Service, codecs: [GRPC.Codec.Erlpack]

    def reply_hello(req, _stream) do
      {:ok, "Hello, #{req}"}
    end
  end

  defmodule HelloErlpackStub do
    use GRPC.Stub, service: Helloworld.Notypes.Service
  end

  test "Says hello over erlpack" do
    run_server(HelloServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect(
          "localhost:#{port}",
          interceptors: [GRPC.Logger.Client],
          codec: GRPC.Codec.Erlpack
        )

      name = "World"
      {:ok, reply} = channel |> HelloErlpackStub.reply_hello(name)
      assert reply == {:ok, "Hello, #{name}"}
    end)
  end

  test "Says hello over erlpack call level" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Logger.Client])
      name = "World"
      {:ok, reply} = channel |> HelloErlpackStub.reply_hello(name, codec: GRPC.Codec.Erlpack)
      assert reply == {:ok, "Hello, #{name}"}
    end)
  end
end

defmodule GRPC.Integration.ErplackNotypesTest do
  use GRPC.Integration.TestCase, async: true

  defmodule Helloworld.Notypes.Service do
    use GRPC.Service, name: "helloworld.Notypes"

    rpc :ReplyHello, :ignore, :ignore
  end

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Notypes.Service, codec: GRPC.Codec.Erlpack

    def reply_hello(req, _stream) do
      {:ok, "Hello, #{req}"}
    end
  end

  defmodule HelloErlpackStub do
    use GRPC.Stub, service: Helloworld.Notypes.Service, codec: GRPC.Codec.Erlpack
  end

  test "Says hello over erlpack" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", interceptors: [GRPC.Logger.Client])
      name = "World"
      {:ok, reply} = channel |> HelloErlpackStub.reply_hello(name)
      assert reply == {:ok, "Hello, #{name}"}
    end)
  end
end

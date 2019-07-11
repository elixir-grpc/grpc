defmodule GRPC.ServerTest do
  use ExUnit.Case

  defmodule Greeter.Service do
    use GRPC.Service, name: "hello"
  end

  defmodule Greeter.Server do
    use GRPC.Server, service: Greeter.Service
  end

  test "stop/2 works" do
    assert {nil, %{"hello" => GRPC.ServerTest.Greeter.Server}} =
             GRPC.Server.stop(Greeter.Server, adapter: GRPC.Test.ServerAdapter)
  end

  test "send_reply/2 works" do
    stream = %GRPC.Server.Stream{adapter: GRPC.Test.ServerAdapter, codec: GRPC.Codec.Erlpack}
    response = <<1, 2, 3, 4, 5, 6, 7, 8>>
    assert %GRPC.Server.Stream{} = GRPC.Server.send_reply(stream, response)
  end
end

defmodule GRPC.ServerTest do
  use ExUnit.Case, async: true

  test "stop/2 works" do
    assert {Greeter.Server} = GRPC.Server.stop(Greeter.Server, adapter: GRPC.Test.ServerAdapter)
  end

  test "stream_send/2 works" do
    stream = %GRPC.Server.Stream{adapter: GRPC.Test.ServerAdapter, marshal: &(&1)}
    response = <<1, 2, 3, 4, 5, 6, 7, 8>>
    assert {^stream, [<<0>>, <<0, 0, 0, 8>>, <<1, 2, 3, 4, 5, 6, 7, 8>>]} =
      GRPC.Server.stream_send(stream, response)
  end
end

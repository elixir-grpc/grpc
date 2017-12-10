defmodule GRPC.ServerTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  defmodule A do
    def stream_send(stream, response, next) do
      IO.write("A")
      next.(stream, response)
    end
  end

  defmodule B do
    def stream_send(stream, response, next) do
      IO.write("B")
      next.(stream, response)
    end
  end

  defmodule Greeter.Service do
    use GRPC.Service, name: "hello"
  end

  defmodule Greeter.Server do
    use GRPC.Server, service: Greeter.Service, middlewares: [A, B]
  end

  test "stop/2 works" do
    assert {%{"hello" => GRPC.ServerTest.Greeter.Server}} = GRPC.Server.stop(Greeter.Server, adapter: GRPC.Test.ServerAdapter)
  end

  test "stream_send/2 works" do
    assert capture_io(fn ->
      stream = %GRPC.Server.Stream{adapter: GRPC.Test.ServerAdapter, marshal: &(&1)}
      response = <<1, 2, 3, 4, 5, 6, 7, 8>>
      assert {:ok, 13} == Greeter.Server.stream_send(stream, response)
    end) == "BA"
  end
end

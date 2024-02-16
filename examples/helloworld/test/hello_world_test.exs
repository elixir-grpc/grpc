defmodule HelloworldTest do
  @moduledoc false

  use ExUnit.Case

  setup_all do
    {:ok, channel} = GRPC.Stub.connect("localhost:50051", interceptors: [GRPC.Client.Interceptors.Logger])
    [channel: channel]
  end

  test "helloworld should be successful", %{channel: channel} do
    req = Helloworld.HelloRequest.new(name: "grpc-elixir")
    assert {:ok, %{message: msg, today: _}} = Helloworld.Greeter.Stub.say_hello(channel, req)
    assert msg == "Hello grpc-elixir"
  end
end

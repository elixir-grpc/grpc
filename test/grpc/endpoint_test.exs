defmodule GRPC.EndpointTest do
  use ExUnit.Case, async: true

  defmodule FooEndpoint do
    use GRPC.Endpoint

    intercept Interceptor1
    intercept Interceptor2, foo: 1

    run Server1, interceptors: [Interceptor3]
    run [Server2, Server3], interceptors: [{Interceptor4, []}]
  end

  test "intercept works" do
    assert [Interceptor1, {Interceptor2, [foo: 1]}] == FooEndpoint.__metadata__(:interceptors)
  end

  test "run creates servers" do
    assert [Server2, Server3, Server1] == FooEndpoint.__metadata__(:servers)
  end

  test "run creates server_interceptors" do
    mw = [{Interceptor4, []}]
    assert %{Server1 => [Interceptor3], Server2 => mw, Server3 => mw} == FooEndpoint.__metadata__(:server_interceptors)
  end
end

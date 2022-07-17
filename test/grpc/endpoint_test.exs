defmodule GRPC.EndpointTest do
  use ExUnit.Case, async: true

  defmodule Interceptor1 do
    def init(_), do: nil
  end

  defmodule Interceptor2 do
    def init(opts), do: opts
  end

  defmodule Interceptor3 do
    def init(_), do: [foo: :bar]
  end

  defmodule Interceptor4 do
    def init(opts), do: opts
  end

  defmodule FooEndpoint do
    use GRPC.Endpoint

    intercept Interceptor1
    intercept Interceptor2, foo: 1

    run Server1, interceptors: [Interceptor3]
    run [Server2, Server3], interceptors: [{Interceptor4, []}]
  end

  test "intercept works" do
    assert [{Interceptor1, nil}, {Interceptor2, [foo: 1]}] == FooEndpoint.__meta__(:interceptors)
  end

  test "run creates servers" do
    assert [Server2, Server3, Server1] == FooEndpoint.__meta__(:servers)
  end

  test "run creates server_interceptors" do
    mw = [{Interceptor4, []}]

    assert %{Server1 => [{Interceptor3, [foo: :bar]}], Server2 => mw, Server3 => mw} ==
             FooEndpoint.__meta__(:server_interceptors)
  end
end

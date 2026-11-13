defmodule GRPC.Server.GrpcWebHttp1Test do
  # `resolve_access_mode/3` has a clause that matches `%{version: "HTTP/1.1"}`. Cowboy
  # stores the version as the atom `:"HTTP/1.1"`, so it never matches. GHSA-xc5v-89vq-hv5m
  # suggests fixing it. Fixed, it would send every HTTP/1.1 request, gRPC-web ones from
  # a browser included, to transcoding, and they would fail with 400. This test fails
  # if someone makes that change without a replacement.
  use ExUnit.Case, async: false

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: "Hello, #{req.name}"}
  end

  test "a gRPC-web request over HTTP/1.1 to a plain gRPC route is served" do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [HelloServer, 0, []]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    on_exit(fn -> GRPC.Server.stop(HelloServer) end)

    proto = Protobuf.encode(%Helloworld.HelloRequest{name: "web"})
    body = <<0::8, byte_size(proto)::32, proto::binary>>

    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [:http]})
    {:ok, :http} = :gun.await_up(conn, 5_000)

    ref =
      :gun.post(
        conn,
        "/helloworld.Greeter/SayHello",
        [{"content-type", "application/grpc-web+proto"}],
        body
      )

    assert {:response, :nofin, 200, _headers} = :gun.await(conn, ref, 3_000)
    assert {:ok, reply} = :gun.await_body(conn, ref, 3_000)
    assert reply =~ "Hello, web"
    :gun.close(conn)
  end
end

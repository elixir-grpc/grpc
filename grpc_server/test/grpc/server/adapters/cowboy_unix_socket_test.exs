defmodule GRPC.Server.Adapters.CowboyUnixSocketTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  defmodule Greeter do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(%{name: name}, _stream), do: %Helloworld.HelloReply{message: "Hello #{name}"}
  end

  @reply_timeout_ms 5_000

  setup do
    path = Path.join(System.tmp_dir!(), "grpc_#{System.unique_integer([:positive])}.sock")
    on_exit(fn -> File.rm(path) end)
    %{path: path}
  end

  test "the supervisor serves on a unix socket and logs its path", %{path: path} do
    log =
      capture_log(fn ->
        start_supervised!(
          {GRPC.Server.Supervisor,
           servers: [Greeter], port: 0, start_server: true, adapter_opts: [ip: {:local, path}]}
        )
      end)

    assert log =~ "Running #{inspect(Greeter)} with Cowboy using http on unix:#{path}"
    assert say_hello(path, "unix") == %Helloworld.HelloReply{message: "Hello unix"}
  end

  defp say_hello(path, name) do
    {:ok, conn} = :gun.open_unix(String.to_charlist(path), %{protocols: [:http2]})
    on_exit(fn -> :gun.close(conn) end)
    {:ok, :http2} = :gun.await_up(conn, @reply_timeout_ms)

    message = Protobuf.encode(%Helloworld.HelloRequest{name: name})
    headers = [{"content-type", "application/grpc"}, {"te", "trailers"}]
    body = <<0, byte_size(message)::32, message::binary>>
    ref = :gun.post(conn, "/helloworld.Greeter/SayHello", headers, body)

    {:response, :nofin, 200, _headers} = :gun.await(conn, ref, @reply_timeout_ms)

    {:ok, <<0, size::32, reply::binary-size(size)>>, trailers} =
      :gun.await_body(conn, ref, @reply_timeout_ms)

    assert {"grpc-status", "0"} in trailers
    Protobuf.decode(reply, Helloworld.HelloReply)
  end
end

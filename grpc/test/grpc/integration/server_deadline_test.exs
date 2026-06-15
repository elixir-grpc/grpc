defmodule GRPC.Integration.ServerDeadlineTest do
  use GRPC.Integration.TestCase

  # End-to-end coverage for the inbound deadline surfaced on `GRPC.Server.Stream`.
  #
  # SKIPPED in normal CI: this app depends on the published `grpc_server` hex
  # package (see mix.exs), which does not yet carry the `:deadline` field. To run
  # it, point `grpc` at the local source by toggling the `grpc_server` dep in
  # mix.exs to `{:grpc_server, path: "../grpc_server", only: :test}` and re-run
  # `mix deps.get`. The `remaining_ms/1` helper itself is unit-tested in
  # `grpc_server/test/grpc/server/stream_test.exs`.
  @moduletag :skip

  # Echoes back what the server observed about the inbound deadline so the test
  # can assert it end-to-end. `Map.get/2` (rather than `stream.deadline`) keeps
  # this compilable against a `grpc_server` build without the field.
  defmodule DeadlineServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(_req, stream) do
      deadline = Map.get(stream, :deadline)
      kind = if is_integer(deadline), do: "set", else: "nil"
      %Helloworld.HelloReply{message: "#{kind}:#{inspect(deadline)}"}
    end
  end

  test "stream.deadline is populated from the client's timeout" do
    run_server(DeadlineServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "Elixir"}

      {:ok, reply} = Helloworld.Greeter.Stub.say_hello(channel, req, timeout: 500)

      assert String.starts_with?(reply.message, "set:")
    end)
  end

  test "stream.deadline is nil when the client sends no grpc-timeout header" do
    run_server(DeadlineServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "Elixir"}

      # `:infinity` makes the client omit the `grpc-timeout` header entirely.
      {:ok, reply} = Helloworld.Greeter.Stub.say_hello(channel, req, timeout: :infinity)

      assert reply.message == "nil:nil"
    end)
  end
end

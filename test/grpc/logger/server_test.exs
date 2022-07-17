defmodule GRPC.Logger.ServerTest do
  use ExUnit.Case, async: true

  alias GRPC.Logger.Server

  alias GRPC.Server.Stream

  test "request id is only set if not previously set" do
    assert Logger.metadata() == []

    request_id = to_string(System.monotonic_time())
    stream = %Stream{server: :server, rpc: {1, 2, 3}, request_id: request_id}
    Server.call(:request, stream, fn :request, ^stream -> {:ok, :ok} end, level: :info)

    assert [request_id: request_id] == Logger.metadata()

    stream = %{stream | request_id: nil}
    Server.call(:request, stream, fn :request, ^stream -> {:ok, :ok} end, level: :info)
    assert request_id == Logger.metadata()[:request_id]
  end
end

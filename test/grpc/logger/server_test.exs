defmodule GRPC.Logger.ServerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias GRPC.Logger.Server

  alias GRPC.Server.Stream

  test "request id is only set if not previously set" do
    assert Logger.metadata() == []

    request_id = to_string(System.monotonic_time())
    stream = %Stream{server: :server, rpc: {1, 2, 3}, request_id: request_id}

    Server.call(
      :request,
      stream,
      fn :request, ^stream -> {:ok, :ok} end,
      Server.init(level: :info)
    )

    assert [request_id: request_id] == Logger.metadata()

    stream = %{stream | request_id: nil}

    Server.call(
      :request,
      stream,
      fn :request, ^stream -> {:ok, :ok} end,
      Server.init(level: :info)
    )

    assert request_id == Logger.metadata()[:request_id]
  end

  test "accepted_comparators filter logs correctly" do
    for {configured_level, accepted_comparators, should_log} <-
          [
            {:error, [:lt], false},
            {:error, [:eq], false},
            {:error, [:gt], true},
            {:debug, [:eq], false},
            {:debug, [:eq, :gt], false},
            {:info, [:lt, :eq], true}
          ] do
      server_name = :"server_#{System.unique_integer()}"

      logger_level = Logger.level()
      assert logger_level == :info

      logs =
        capture_log(fn ->
          stream = %Stream{server: server_name, rpc: {1, 2, 3}, request_id: "1234"}

          Server.call(
            :request,
            stream,
            fn :request, ^stream -> {:ok, :ok} end,
            Server.init(
              level: configured_level,
              accepted_comparators: accepted_comparators
            )
          )
        end)

      if should_log do
        assert Regex.match?(
                 ~r/\[#{configured_level}\]\s+Handled by #{inspect(server_name)}/,
                 logs
               )
      else
        assert logs == ""
      end
    end
  end
end

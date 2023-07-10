defmodule GRPC.Server.Interceptors.LoggerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias GRPC.Server.Interceptors.Logger, as: LoggerInterceptor
  alias GRPC.Server.Stream

  defmodule FakeRequest do
    defstruct []
  end

  @server_name :server
  @rpc {1, 2, 3}

  setup do
    log_level = Logger.level()
    on_exit(fn -> Logger.configure(level: log_level) end)
  end

  test "request id is only set if not previously set" do
    assert Logger.metadata() == []

    request_id = to_string(System.monotonic_time())
    request = %FakeRequest{}
    stream = %Stream{server: @server_name, rpc: @rpc, request_id: request_id}

    LoggerInterceptor.call(
      request,
      stream,
      fn ^request, ^stream -> {:ok, :ok} end,
      LoggerInterceptor.init(level: :info)
    )

    assert [request_id: request_id] == Logger.metadata()

    stream = %{stream | request_id: nil}

    LoggerInterceptor.call(
      :request,
      stream,
      fn :request, ^stream -> {:ok, :ok} end,
      LoggerInterceptor.init(level: :info)
    )

    assert request_id == Logger.metadata()[:request_id]
  end

  test "logs info-level by default" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{server: @server_name, rpc: @rpc, request_id: nil}
    next = fn _stream, _request -> {:ok, :ok} end
    opts = LoggerInterceptor.init([])

    logs =
      capture_log(fn ->
        LoggerInterceptor.call(request, stream, next, opts)
      end)

    assert logs =~ ~r/\[info\]\s+Handled by #{inspect(@server_name)}/
  end

  test "allows customizing log level" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{server: @server_name, rpc: @rpc, request_id: nil}
    next = fn _stream, _request -> {:ok, :ok} end
    opts = LoggerInterceptor.init(level: :warn)

    logs =
      capture_log(fn ->
        LoggerInterceptor.call(request, stream, next, opts)
      end)

    assert logs =~ ~r/\[warn(?:ing)?\]\s+Handled by #{inspect(@server_name)}/
  end

  @tag capture_log: true
  test "calls next when above :logger level" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{server: @server_name, rpc: @rpc, request_id: nil}
    next = fn stream, req -> send(self(), {:next_called, stream, req}) end
    opts = LoggerInterceptor.init(level: :info)

    LoggerInterceptor.call(request, stream, next, opts)

    assert_receive {:next_called, ^request, ^stream}
  end

  test "calls next when below :logger level" do
    Logger.configure(level: :warn)

    request = %FakeRequest{}
    stream = %Stream{server: @server_name, rpc: @rpc, request_id: nil}
    next = fn stream, req -> send(self(), {:next_called, stream, req}) end
    opts = LoggerInterceptor.init(level: :info)

    LoggerInterceptor.call(request, stream, next, opts)

    assert_receive {:next_called, ^request, ^stream}
  end
end

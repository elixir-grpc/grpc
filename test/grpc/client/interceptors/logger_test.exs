defmodule GRPC.Client.Interceptors.LoggerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias GRPC.Client.Interceptors.Logger, as: LoggerInterceptor
  alias GRPC.Client.Stream

  defmodule FakeRequest do
    defstruct []
  end

  @service_name "service_name"
  @rpc {1, 2, 3}

  setup do
    log_level = Logger.level()
    on_exit(fn -> Logger.configure(level: log_level) end)
  end

  test "logs info-level by default" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{grpc_type: :unary, rpc: @rpc, service_name: @service_name}
    next = fn _stream, _request -> {:ok, :ok} end
    opts = LoggerInterceptor.init([])

    logs =
      capture_log(fn ->
        LoggerInterceptor.call(stream, request, next, opts)
      end)

    assert logs =~ ~r/\[info\]\s+Call #{to_string(elem(@rpc, 0))} of #{@service_name}/
  end

  test "allows customizing log level" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{grpc_type: :unary, rpc: @rpc, service_name: @service_name}
    next = fn _stream, _request -> {:ok, :ok} end
    opts = LoggerInterceptor.init(level: :warn)

    logs =
      capture_log(fn ->
        LoggerInterceptor.call(stream, request, next, opts)
      end)

    assert logs =~ ~r/\[warn(?:ing)?\]\s+Call #{to_string(elem(@rpc, 0))} of #{@service_name}/
  end

  @tag capture_log: true
  test "calls next when above :logger level" do
    Logger.configure(level: :all)

    request = %FakeRequest{}
    stream = %Stream{grpc_type: :unary, rpc: @rpc, service_name: @service_name}
    next = fn stream, req -> send(self(), {:next_called, stream, req}) end
    opts = LoggerInterceptor.init(level: :info)

    LoggerInterceptor.call(stream, request, next, opts)

    assert_receive {:next_called, ^stream, ^request}
  end

  test "calls next when below :logger level" do
    Logger.configure(level: :warn)

    request = %FakeRequest{}
    stream = %Stream{grpc_type: :unary, rpc: @rpc, service_name: @service_name}
    next = fn stream, req -> send(self(), {:next_called, stream, req}) end
    opts = LoggerInterceptor.init(level: :info)

    LoggerInterceptor.call(stream, request, next, opts)

    assert_receive {:next_called, ^stream, ^request}
  end
end

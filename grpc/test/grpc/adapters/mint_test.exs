defmodule GRPC.Client.Adapters.MintTest do
  use GRPC.Client.DataCase, async: false

  alias Elixir.Mint.HTTPError
  alias Elixir.Mint.TransportError
  alias GRPC.Client.Adapters.Mint

  defmodule TLSEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup do
    {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)

    on_exit(fn ->
      :ok = GRPC.Server.stop(FeatureServer)
    end)

    %{port: port}
  end

  describe "connect/2" do
    test "connects insecurely (default options)", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result
    end

    test "connects insecurely (custom options)", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, transport_opts: [ip: :loopback])
      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options breaks things
      assert {:error, :badarg} = Mint.connect(channel, transport_opts: [ip: "256.0.0.0"])
    end

    test "accepts config_options for application specific configuration", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} =
               Mint.connect(channel, config_options: [transport_opts: [ip: :loopback]])

      assert %{channel | adapter_payload: %{conn_pid: result.adapter_payload.conn_pid}} == result

      # Ensure that changing one of the options via config_options also breaks things
      assert {:error, :badarg} =
               Mint.connect(channel, config_options: [transport_opts: [ip: "256.0.0.0"]])
    end

    test "defaults client settings when none is passed", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} = Mint.connect(channel, [])
      state = :sys.get_state(result.adapter_payload.conn_pid)

      # Mint mirrors advertised client_settings onto conn at connect time.
      assert state.connect_opts[:client_settings] == [
               initial_window_size: 8_000_000,
               max_frame_size: 8_000_000
             ]

      assert %{initial_window_size: 8_000_000, max_frame_size: 8_000_000} =
               Map.take(state.conn.client_settings, [:initial_window_size, :max_frame_size])
    end

    test "allow client settings to be passed", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:ok, result} =
               Mint.connect(channel,
                 client_settings: [
                   initial_window_size: 50_000,
                   max_frame_size: 50_000
                 ]
               )

      state = :sys.get_state(result.adapter_payload.conn_pid)

      assert state.connect_opts[:client_settings] == [
               initial_window_size: 50_000,
               max_frame_size: 50_000
             ]

      assert %{initial_window_size: 50_000, max_frame_size: 50_000} =
               Map.take(state.conn.client_settings, [:initial_window_size, :max_frame_size])
    end
  end

  describe "connect/2 - keepalive" do
    setup(%{port: port}) do
      %{channel: build(:channel, adapter: Mint, port: port, host: "localhost")}
    end

    test "is disabled by default", %{channel: channel} do
      assert {:ok, result} = Mint.connect(channel, [])

      assert %{keepalive: nil, keepalive_tolerance: nil} =
               :sys.get_state(result.adapter_payload.conn_pid)
    end

    test "passes keepalive options to the connection process", %{channel: channel} do
      keepalive = 15_000
      keepalive_tolerance = 3

      assert {:ok, result} =
               Mint.connect(channel,
                 keepalive: keepalive,
                 keepalive_tolerance: keepalive_tolerance
               )

      assert %{keepalive: ^keepalive, keepalive_tolerance: ^keepalive_tolerance} =
               :sys.get_state(result.adapter_payload.conn_pid)
    end
  end

  describe "disconnect/1" do
    test "keeps adapter_payload as a map with conn_pid set to nil", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      assert %{conn_pid: pid} = connected.adapter_payload
      assert is_pid(pid)

      {:ok, disconnected} = Mint.disconnect(connected)

      assert %{conn_pid: nil} = disconnected.adapter_payload
    end

    test "disconnect is idempotent — calling it twice succeeds", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      {:ok, disconnected} = Mint.disconnect(connected)
      {:ok, disconnected_again} = Mint.disconnect(disconnected)

      assert %{conn_pid: nil} = disconnected_again.adapter_payload
    end

    test "send_request/3 raises ArgumentError when conn_pid is nil", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      {:ok, disconnected} = Mint.disconnect(connected)

      stream = build(:client_stream, channel: disconnected)

      assert_raise ArgumentError, ~r/Can't perform a request without a connection process/, fn ->
        Mint.send_request(stream, %Helloworld.HelloRequest{name: "test"}, [])
      end
    end
  end

  describe "handle_errors_receive_data/2" do
    setup do
      {:ok, stream_response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      %{stream_response_pid: stream_response_pid}
    end

    test "returns UNAVAILABLE when the connection is gone and stops the stream response process",
         %{stream_response_pid: pid} do
      stream =
        build(:client_stream, payload: %{response: {:error, :closed}, stream_response_pid: pid})

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.handle_errors_receive_data(stream, [])

      assert status == GRPC.Status.unavailable()
      assert message == "the connection is closed"

      refute Process.alive?(pid)
    end

    test "returns UNAVAILABLE for a transport error", %{stream_response_pid: pid} do
      error = %TransportError{reason: :econnrefused}

      stream =
        build(:client_stream, payload: %{response: {:error, error}, stream_response_pid: pid})

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.handle_errors_receive_data(stream, [])

      assert status == GRPC.Status.unavailable()
      assert message == Exception.message(error)
    end

    test "returns UNAVAILABLE when the server closed the connection", %{
      stream_response_pid: pid
    } do
      error = %HTTPError{
        module: Elixir.Mint.HTTP2,
        reason: {:server_closed_connection, :no_error, "shutting down"}
      }

      stream =
        build(:client_stream, payload: %{response: {:error, error}, stream_response_pid: pid})

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.handle_errors_receive_data(stream, [])

      assert status == GRPC.Status.unavailable()
      assert message == Exception.message(error)
    end

    test "passes an existing GRPC.RPCError through untouched", %{stream_response_pid: pid} do
      error = GRPC.RPCError.exception(GRPC.Status.unavailable(), "the connection is closed")

      stream =
        build(:client_stream, payload: %{response: {:error, error}, stream_response_pid: pid})

      assert {:error, ^error} = Mint.handle_errors_receive_data(stream, [])
    end

    test "returns UNKNOWN for a reason it cannot classify and stops the stream response process",
         %{stream_response_pid: pid} do
      response = {:error, :something_else}
      stream = build(:client_stream, payload: %{response: response, stream_response_pid: pid})

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.handle_errors_receive_data(stream, [])

      assert status == GRPC.Status.unknown()
      assert message == "error occurred while receiving data: #{inspect(response)}"

      refute Process.alive?(pid)
    end

    test "classifies a remote RPC error without changing it" do
      attach_telemetry([:grpc, :client, :rpc, :stop])
      error = GRPC.RPCError.exception(GRPC.Status.internal(), "remote error")

      {:ok, response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.consume(response_pid, :error, error)
      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.done(response_pid)

      stream =
        build(:client_stream,
          payload: %{
            response: {:ok, %{request_ref: make_ref()}},
            stream_response_pid: response_pid
          }
        )

      assert {:error, ^error} =
               GRPC.Client.Telemetry.client_span(stream, :request, fn ->
                 Mint.receive_data(stream, [])
               end)

      assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                      %{failure_stage: :remote, failure_reason: nil}}
    end

    test "classifies a closed connection without changing the error" do
      attach_telemetry([:grpc, :client, :rpc, :stop])

      error = GRPC.RPCError.exception(GRPC.Status.unavailable(), "the connection is closed")

      {:ok, response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      :ok =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.consume(response_pid, :error, error)

      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.done(response_pid)

      stream =
        build(:client_stream,
          payload: %{
            response: {:ok, %{request_ref: make_ref()}},
            stream_response_pid: response_pid
          }
        )

      assert {:error, ^error} =
               GRPC.Client.Telemetry.client_span(stream, :request, fn ->
                 Mint.receive_data(stream, [])
               end)

      assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                      %{
                        failure_stage: :after_dispatch,
                        failure_reason: :connection_unavailable
                      }}
    end
  end

  describe "receive_data/2 - connection errors from the stream response process" do
    @describetag timeout: 5_000

    test "classifies a server-closed connection as UNAVAILABLE and connection_unavailable" do
      attach_telemetry([:grpc, :client, :rpc, :stop])

      error = %HTTPError{
        module: Elixir.Mint.HTTP2,
        reason: {:server_closed_connection, :no_error, "shutting down"}
      }

      {:ok, response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.consume(response_pid, :error, error)
      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.done(response_pid)

      stream =
        build(:client_stream,
          payload: %{
            response: {:ok, %{request_ref: make_ref()}},
            stream_response_pid: response_pid
          }
        )

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               GRPC.Client.Telemetry.client_span(stream, :request, fn ->
                 Mint.receive_data(stream, [])
               end)

      assert status == GRPC.Status.unavailable()
      assert message == Exception.message(error)

      assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                      %{
                        failure_stage: :after_dispatch,
                        failure_reason: :connection_unavailable
                      }}
    end
  end

  describe "receive_data/2 - request ended by the connection mid-request" do
    @describetag timeout: 5_000

    setup do
      {:ok, stream_response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      stream =
        build(:client_stream,
          payload: %{
            stream_response_pid: stream_response_pid,
            response: {:ok, %{request_ref: make_ref()}}
          }
        )

      %{stream: stream, stream_response_pid: stream_response_pid}
    end

    test "returns the UNAVAILABLE error the connection process produced", %{
      stream: stream,
      stream_response_pid: pid
    } do
      error = GRPC.RPCError.exception(GRPC.Status.unavailable(), "the connection is closed")

      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.consume(pid, :error, error)
      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.done(pid)

      assert {:error, ^error} = Mint.receive_data(stream, [])
    end

    test "normalises a raw transport error into an UNAVAILABLE GRPC.RPCError", %{
      stream: stream,
      stream_response_pid: pid
    } do
      error = %TransportError{reason: :closed}

      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.consume(pid, :error, error)
      :ok = GRPC.Client.Adapters.Mint.StreamResponseProcess.done(pid)

      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.receive_data(stream, [])

      assert status == GRPC.Status.unavailable()
      assert message == Exception.message(error)
    end
  end

  describe "receive_data/2 - deadline" do
    setup do
      {:ok, stream_response_pid} =
        GRPC.Client.Adapters.Mint.StreamResponseProcess.start_link(build(:client_stream), true)

      # A connection process that is already gone: nothing will ever notify the
      # stream response process, which is what used to block the caller forever.
      dead_conn_pid = spawn(fn -> :ok end)
      ref = Process.monitor(dead_conn_pid)
      assert_receive {:DOWN, ^ref, :process, ^dead_conn_pid, _reason}

      stream =
        build(:client_stream,
          channel: build(:channel, adapter: Mint, adapter_payload: %{conn_pid: dead_conn_pid}),
          payload: %{
            stream_response_pid: stream_response_pid,
            response: {:ok, %{request_ref: make_ref()}}
          }
        )

      %{stream: stream, stream_response_pid: stream_response_pid}
    end

    # Without a deadline these would block until the ExUnit timeout, so keep that
    # wait short enough to read as a failure rather than as a stuck suite.
    @describetag timeout: 5_000

    test "returns DEADLINE_EXCEEDED when no response arrives in time", %{stream: stream} do
      assert {:error, %GRPC.RPCError{status: status, message: message}} =
               Mint.receive_data(stream, timeout: 10)

      assert status == GRPC.Status.deadline_exceeded()
      assert message == "deadline exceeded"
    end

    test "stops the stream response process it gave up on", %{
      stream: stream,
      stream_response_pid: stream_response_pid
    } do
      assert {:error, %GRPC.RPCError{}} = Mint.receive_data(stream, timeout: 10)

      refute Process.alive?(stream_response_pid)
    end

    test "accepts the milliseconds a :deadline is resolved into", %{stream: stream} do
      timeout = GRPC.TimeUtils.to_relative(DateTime.add(DateTime.utc_now(), 20, :millisecond))

      assert is_number(timeout)

      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: timeout)

      assert status == GRPC.Status.deadline_exceeded()
    end

    test "treats a deadline that has already passed as an immediate one", %{stream: stream} do
      timeout = GRPC.TimeUtils.to_relative(DateTime.add(DateTime.utc_now(), -5, :second))

      assert timeout < 0

      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: timeout)

      assert status == GRPC.Status.deadline_exceeded()
    end

    test "lets an explicit :deadline override the timeout GRPC.Stub fills in", %{stream: stream} do
      assert {:error, %GRPC.RPCError{status: status}} =
               Mint.receive_data(stream, timeout: :timer.minutes(1), deadline: 10)

      assert status == GRPC.Status.deadline_exceeded()
    end
  end

  describe "receive_data/2 - deadline through GRPC.Stub" do
    test "a :deadline on a unary call reaches the server instead of raising", %{port: port} do
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: Mint)
      on_exit(fn -> GRPC.Stub.disconnect(channel) end)

      point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

      assert {:ok, feature} =
               Routeguide.RouteGuide.Stub.get_feature(channel, point,
                 deadline: DateTime.add(DateTime.utc_now(), 30, :second)
               )

      assert feature == %Routeguide.Feature{location: point, name: "409146138,-746188906"}
    end
  end

  describe "connect/2 with retry option" do
    test "passes retry option to ConnectionProcess state", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, retry: 5)
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.retry == 5
      assert state.retry_attempt == 0
      assert state.scheme == :http
      assert state.host == "localhost"
      assert state.port == port
    end

    test "defaults retry to 0 when not specified", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, [])
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.retry == 0
    end

    test "accepts :infinity as the retry budget", %{port: port} do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      {:ok, connected} = Mint.connect(channel, retry: :infinity)
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.retry == :infinity
    end

    test "rejects a retry budget that is neither a non-negative integer nor :infinity", %{
      port: port
    } do
      channel = build(:channel, adapter: Mint, port: port, host: "localhost")

      assert {:error, message} = Mint.connect(channel, retry: -1)
      assert message =~ ":retry must be a non-negative integer or :infinity"

      assert {:error, message} = Mint.connect(channel, retry: :forever)
      assert message =~ ":retry must be a non-negative integer or :infinity"
    end
  end

  describe "validate_opts/1" do
    test "accepts an unset retry, a non-negative integer or :infinity" do
      assert :ok == Mint.validate_opts([])
      assert :ok == Mint.validate_opts(retry: nil)
      assert :ok == Mint.validate_opts(retry: 0)
      assert :ok == Mint.validate_opts(retry: 3)
      assert :ok == Mint.validate_opts(retry: :infinity)
    end

    test "returns an error for anything else" do
      assert {:error, message} = Mint.validate_opts(retry: -1)
      assert message =~ ":retry must be a non-negative integer or :infinity"

      assert {:error, message} = Mint.validate_opts(retry: :forever)
      assert message =~ ":retry must be a non-negative integer or :infinity"
    end

    test "accepts every option the adapter reads, and the one the pool adds" do
      assert :ok ==
               Mint.validate_opts(
                 retry: 1,
                 transport_opts: [nodelay: false],
                 client_settings: [max_frame_size: 16_384],
                 keepalive: 30_000,
                 keepalive_tolerance: 2,
                 config_options: [],
                 logical_connection_ref: make_ref()
               )
    end

    test "rejects an unknown option and lists the valid ones" do
      assert {:error, message} = Mint.validate_opts(certfile: "client.pem")
      assert message =~ "unknown option(s) for the Mint adapter: [:certfile]"
      assert message =~ ":transport_opts"
      assert message =~ ":keepalive_tolerance"
      refute message =~ ":logical_connection_ref"
    end

    test "points :custom_opts at :cred" do
      assert {:error, message} =
               Mint.validate_opts(custom_opts: [certfile: "client.pem"], retry: 1)

      assert message =~ "[:custom_opts]"
      assert message =~ ":cred"
    end

    test "GRPC.Stub.connect/2 raises for an unknown adapter option", %{port: port} do
      assert_raise ArgumentError,
                   ~r/unknown option\(s\) for the Mint adapter: \[:custom_opts\]/,
                   fn ->
                     GRPC.Stub.connect("localhost:#{port}",
                       adapter: Mint,
                       adapter_opts: [custom_opts: [certfile: "client.pem"]]
                     )
                   end
    end
  end

  describe "connect/2 with hostname" do
    setup do
      server_credential = build(:credential)

      {:ok, _, port} =
        GRPC.Server.start_endpoint(TLSEndpoint, 0, adapter_opts: [cred: server_credential])

      on_exit(fn -> :ok = GRPC.Server.stop_endpoint(TLSEndpoint) end)

      %{port: port}
    end

    test "dials the resolved address but hands the hostname to Mint for TLS", %{port: port} do
      channel =
        build(:channel,
          adapter: Mint,
          port: port,
          host: "127.0.0.1",
          hostname: "my-service.local",
          scheme: "https"
        )

      {:ok, connected} = Mint.connect(channel, [])
      state = :sys.get_state(connected.adapter_payload.conn_pid)

      assert state.host == "127.0.0.1"
      assert state.connect_opts[:hostname] == "my-service.local"
    end
  end
end

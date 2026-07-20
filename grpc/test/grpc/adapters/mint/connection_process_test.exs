defmodule GRPC.Client.Adapters.Mint.ConnectionProcessTest do
  use GRPC.Client.DataCase, async: true
  alias GRPC.Client.Adapters.Mint.ConnectionProcess
  alias GRPC.Client.Adapters.Mint.StreamResponseProcess

  import ExUnit.CaptureLog

  # Unique Ranch listener names so this module can run async without racing
  # other suites that still start the shared FeatureServer module directly.
  defmodule Endpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  defmodule ReconnectExhaustedEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  defmodule ReconnectUnavailableEndpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup_all do
    {:ok, _, port} = GRPC.Server.start_endpoint(Endpoint, 0)

    on_exit(fn ->
      :ok = GRPC.Server.stop_endpoint(Endpoint)
    end)

    %{port: port}
  end

  describe "start_link/4" do
    test "non-successful connection stops the connection process without exiting it's caller" do
      Process.flag(:trap_exit, true)

      logs =
        capture_log(fn ->
          assert {:error, %Mint.TransportError{reason: :econnrefused}} ==
                   ConnectionProcess.start_link(:http, "localhost", 12345)
        end)

      assert logs =~ "unable to establish a connection"
    end

    test "connects insecurely (default options)", %{port: port} do
      {:ok, pid} = ConnectionProcess.start_link(:http, "localhost", port)
      assert Process.alive?(pid)
    end
  end

  describe "disconnect/1" do
    test "stop the process when disconnecting", %{port: port} do
      {:ok, pid} = ConnectionProcess.start_link(:http, "localhost", port)
      assert :ok == ConnectionProcess.disconnect(pid)
      refute Process.alive?(pid)
    end

    test "close the connection when disconnect", %{port: port} do
      {:ok, pid} = ConnectionProcess.start_link(:http, "localhost", port)
      state = :sys.get_state(pid)

      assert {:stop, :normal, :ok, new_state} =
               ConnectionProcess.handle_call(:disconnect, nil, state)

      refute Mint.HTTP.open?(new_state.conn)
    end
  end

  describe "handle_call/2 - request - :stream" do
    setup :valid_connection

    test "start stream request and put empty state for ref", %{
      request: {method, path, headers},
      state: state
    } do
      request = {:request, method, path, headers, :stream, [stream_response_pid: self()]}
      response = ConnectionProcess.handle_call(request, nil, state)

      assert {:reply, {:ok, %{request_ref: request_ref}}, new_state} = response
      assert is_reference(request_ref)

      assert %{
               stream_response_pid: self(),
               done: false,
               response: %{}
             } == new_state.requests[request_ref]

      assert state.conn != new_state.conn
    end

    test "returns error when connection is closed", %{
      request: {method, path, headers},
      state: state
    } do
      {:ok, conn} = Mint.HTTP.close(state.conn)
      request = {:request, method, path, headers, :stream, [stream_response_pid: self()]}
      response = ConnectionProcess.handle_call(request, nil, %{state | conn: conn})

      assert {:reply, {:error, error}, new_state} = response
      assert state.conn != new_state.conn
      assert "the connection is closed" == error
    end

    test "returns error response when mint returns an error when starting stream request", %{
      request: {method, path, headers},
      state: state
    } do
      # Simulates the server closing the connection before we update the state
      {:ok, _conn} = Mint.HTTP.close(state.conn)

      request = {:request, method, path, headers, :stream, [stream_response_pid: self()]}
      response = ConnectionProcess.handle_call(request, nil, state)

      assert {:reply, {:error, error}, new_state} = response
      assert state.conn == new_state.conn
      assert %Mint.TransportError{__exception__: true, reason: :closed} == error
    end
  end

  describe "handle_call/2 - request - payload" do
    setup :valid_connection

    test "start stream request, enqueue payload to be process and continue", %{
      request: {method, path, headers},
      state: state
    } do
      body = <<1, 2, 3>>
      request = {:request, method, path, headers, body, [stream_response_pid: self()]}
      response = ConnectionProcess.handle_call(request, nil, state)

      assert {:reply, {:ok, %{request_ref: request_ref}}, new_state,
              {:continue, :process_request_stream_queue}} = response

      assert is_reference(request_ref)

      assert %{
               stream_response_pid: self(),
               done: false,
               response: %{}
             } == new_state.requests[request_ref]

      assert {[{request_ref, body, nil}], []} == new_state.request_stream_queue
      assert state.conn != new_state.conn
    end

    test "returns error response when connection is closed", %{
      request: {method, path, headers},
      state: state
    } do
      {:ok, conn} = Mint.HTTP.close(state.conn)
      body = <<1, 2, 3>>
      request = {:request, method, path, headers, body, [stream_response_pid: self()]}
      response = ConnectionProcess.handle_call(request, nil, %{state | conn: conn})

      assert {:reply, {:error, error}, new_state} = response
      assert state.conn != new_state.conn
      assert "the connection is closed" == error
    end

    test "returns error response when mint returns an error when starting stream request", %{
      request: {method, path, headers},
      state: state
    } do
      body = <<1, 2, 3>>
      request = {:request, method, path, headers, body, [stream_response_pid: self()]}

      # Simulates the server closing the connection before we update the state
      {:ok, _conn} = Mint.HTTP.close(state.conn)

      response = ConnectionProcess.handle_call(request, nil, state)

      assert {:reply, {:error, error}, _new_state} = response
      assert %Mint.TransportError{reason: :closed} == error
    end
  end

  describe "handle_call/2 - stream_body" do
    setup :valid_connection
    setup :valid_stream_request

    test "reply with :ok when stream :eof is successful", %{
      request_ref: request_ref,
      state: state
    } do
      response = ConnectionProcess.handle_call({:stream_body, request_ref, :eof}, nil, state)
      assert {:reply, :ok, new_state} = response
      assert new_state.conn != state.conn
    end

    test "reply with error when stream :eof is errors", %{request_ref: request_ref, state: state} do
      # Simulates the server closing the connection before we update the state
      {:ok, _conn} = Mint.HTTP.close(state.conn)

      response = ConnectionProcess.handle_call({:stream_body, request_ref, :eof}, nil, state)

      assert {:reply, {:error, error}, _new_state} = response
      assert %Mint.TransportError{__exception__: true, reason: :closed} == error
    end

    test "continue to process payload stream", %{request_ref: request_ref, state: state} do
      response =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<1, 2, 3>>}, self(), state)

      assert {:noreply, new_state, {:continue, :process_request_stream_queue}} = response
      assert {[{request_ref, <<1, 2, 3>>, self()}], []} == new_state.request_stream_queue
      assert new_state.conn == state.conn
    end
  end

  describe "handle_call/2 - cancel_request" do
    setup :valid_connection
    setup :valid_stream_request
    setup :valid_stream_response

    test "reply with :ok when canceling the request is successful, also set stream response pid to done and remove request ref from state",
         %{
           request_ref: request_ref,
           stream_response_pid: response_pid,
           state: state
         } do
      response = ConnectionProcess.handle_call({:cancel_request, request_ref}, nil, state)
      assert {:reply, :ok, new_state} = response
      assert %{} == new_state.requests
      response_state = :sys.get_state(response_pid)
      assert :queue.is_empty(response_state.responses)
      assert true == response_state.done
    end

    test "replies :ok and pops the ref when the stream response process is dead",
         %{
           request_ref: request_ref,
           stream_response_pid: response_pid,
           state: state
         } do
      Process.unlink(response_pid)
      monitor = Process.monitor(response_pid)
      Process.exit(response_pid, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^response_pid, :killed}

      response = ConnectionProcess.handle_call({:cancel_request, request_ref}, nil, state)
      assert {:reply, :ok, new_state} = response
      assert %{} == new_state.requests
    end

    test "is a no-op on state.requests when the ref was already popped",
         %{request_ref: request_ref, state: state} do
      {_ref, state_without_ref} = pop_in(state.requests[request_ref])
      assert %{} == state_without_ref.requests

      response =
        ConnectionProcess.handle_call({:cancel_request, request_ref}, nil, state_without_ref)

      assert {:reply, _reply, new_state} = response
      assert %{} == new_state.requests
    end
  end

  describe "handle_continue/2 - :process_stream_queue" do
    setup :valid_connection
    setup :valid_stream_request
    setup :valid_stream_response

    test "do nothing when there is no window_size in the connection", %{
      request_ref: request_ref,
      state: state
    } do
      # hacky to simulate a window size of zero since this is usually updated with the requests interaction
      state = %{state | conn: %{state.conn | send_window_size: 0}}
      # enqueue the payload onto the request queue
      {_, state, _} =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<1, 2, 3>>}, self(), state)

      assert {:noreply, new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      assert new_state == state
    end

    test "(body_size > window_size) chunk payload stream what is possible and enqueue the rest at the begining of the queue to give priority to the current request",
         %{request_ref: request_ref, state: state} do
      # hacky to simulate a window size of 2 bytes since this is usually updated with the requests interaction
      state = %{state | conn: %{state.conn | send_window_size: 2}}
      # enqueue the payload onto the request queue. Add to items to the queue,
      # this way we can check is the rest is of the payload goes to the first position to the queue
      {_, state, _} =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<1, 2, 3>>}, self(), state)

      {_, state, _} =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<4, 5, 6>>}, self(), state)

      assert {:noreply, new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      # mint update window_size for us.
      # This is how we check if the body was streamed
      assert new_state.conn.send_window_size == 0
      assert {{:value, head_of_queue}, queue} = :queue.out(new_state.request_stream_queue)
      assert {{:value, rest}, {[], []}} = :queue.out(queue)

      # <<1, 2, 3>> got enqueue first, we streamed 2 bytes, now we have only one left <<3>>
      assert {request_ref, <<3>>, self()} == head_of_queue

      # Next to be processed is <<4, 5, 6>>
      assert {request_ref, <<4, 5, 6>>, self()} == rest
    end

    test "(window_size >= body_size) stream bod, send end_stream message and reply caller process (when process ref is present)",
         %{request_ref: request_ref, state: state} do
      {_, state, _} =
        ConnectionProcess.handle_call(
          {:stream_body, request_ref, <<1, 2, 3>>},
          {self(), :tag},
          state
        )

      assert {:noreply, _new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      assert_receive {:tag, :ok}, 500
    end

    test "(window_size >= body_size) stream body, send end_stream message and don't reply caller process (when precess ref is nil)",
         %{request_ref: request_ref, state: state} do
      {_, state, _} =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<1, 2, 3>>}, nil, state)

      assert {:noreply, _new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      refute_receive {:tag, :ok}, 500
    end

    test "(window_size >= body_size) stream body, send end_stream message and check request_queue when queue is not empty",
         %{request_ref: request_ref, state: state} do
      {_, state, _} =
        ConnectionProcess.handle_call(
          {:stream_body, request_ref, <<1, 2, 3>>},
          {self(), :tag},
          state
        )

      {_, state, _} =
        ConnectionProcess.handle_call(
          {:stream_body, request_ref, <<4, 5, 6>>},
          {self(), :tag},
          state
        )

      assert {:noreply, _new_state, {:continue, :process_request_stream_queue}} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      assert_receive {:tag, :ok}, 500
    end

    test "send error to the caller process when server return an error and there is a process ref",
         %{request_ref: request_ref, state: state} do
      # Simulates the server closing the connection before we update the state
      {:ok, _conn} = Mint.HTTP.close(state.conn)

      {_, state, _} =
        ConnectionProcess.handle_call(
          {:stream_body, request_ref, <<1, 2, 3>>},
          {self(), :tag},
          state
        )

      assert {:noreply, _new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      assert_receive {:tag, {:error, %Mint.TransportError{reason: :closed, __exception__: true}}},
                     500
    end

    test "send error message to stream response process when caller process ref is empty",
         %{request_ref: request_ref, state: state, stream_response_pid: response_pid} do
      {_, state, _} =
        ConnectionProcess.handle_call({:stream_body, request_ref, <<1>>}, nil, state)

      # Close connection to simulate an error (like the server closing the connection before we update state)
      {:ok, _conn} = Mint.HTTP.close(state.conn)

      assert {:noreply, _new_state} =
               ConnectionProcess.handle_continue(:process_request_stream_queue, state)

      response_state = :sys.get_state(response_pid)

      assert :queue.to_list(response_state.responses) == [
               error: %Mint.TransportError{reason: :closed, __exception__: true}
             ]
    end
  end

  describe "handle_info - connection_closed - no requests" do
    setup :valid_connection

    test "send a message to parent process to inform the connection is down", %{
      state: state
    } do
      socket = state.conn.socket
      # this is a mocked message to inform the connection is closed
      tcp_message = {:tcp_closed, socket}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state == :closed
      assert_receive {:elixir_grpc, :connection_down, pid}, 500
      assert pid == self()
    end

    test "does not attempt reconnect when retry is 0", %{
      state: state
    } do
      socket = state.conn.socket
      tcp_message = {:tcp_closed, socket}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state == :closed
      assert new_state.retry == 0
      assert_receive {:elixir_grpc, :connection_down, _pid}, 500
    end
  end

  describe "handle_info - connection_closed - with retry" do
    setup :valid_connection_with_retry
    setup :attach_reconnect_telemetry

    test "attempts reconnect when retry > 0 and connection drops", %{
      state: state
    } do
      socket = state.conn.socket
      tcp_message = {:tcp_closed, socket}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state != :closed
      assert new_state.retry_attempt == 0
      refute_receive {:elixir_grpc, :connection_down, _pid}, 200
    end
  end

  describe "handle_info - connection_closed - with retry - exhausted" do
    @describetag private_endpoint: ReconnectExhaustedEndpoint
    setup :start_private_endpoint
    setup :valid_connection_with_retry
    setup :attach_reconnect_telemetry

    test "notifies parent when all retry attempts are exhausted", %{
      state: state,
      port: port,
      process_pid: pid
    } do
      # Close the live client first so Cowboy shutdown does not wait on drain.
      :ok = ConnectionProcess.disconnect(pid)
      :ok = GRPC.Server.stop_endpoint(ReconnectExhaustedEndpoint)

      exhausted_state = %{state | retry: 1, retry_attempt: 1}
      result = ConnectionProcess.handle_info(:reconnect, exhausted_state)
      assert {:noreply, _} = result
      assert_receive {:elixir_grpc, :connection_down, _pid}, 500

      assert_receive {:telemetry, [:grpc, :client, :mint, :reconnect, :exhausted], %{}, metadata}
      assert metadata.attempt == 1
      assert metadata.max == 1
      assert metadata.host == "127.0.0.1"
      assert metadata.port == port
      assert metadata.scheme == :http
    end
  end

  describe "handle_info - connection_closed - with request" do
    setup :valid_connection
    setup :valid_stream_request
    setup :valid_stream_response

    test "send a message to parent process to inform the connection is down and end stream response process",
         %{
           state: state,
           stream_response_pid: response_pid
         } do
      socket = state.conn.socket
      # this is a mocked message to inform the connection is closed
      tcp_message = {:tcp_closed, socket}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state == :closed
      assert_receive {:elixir_grpc, :connection_down, pid}, 500
      response_state = :sys.get_state(response_pid)
      assert :queue.to_list(response_state.responses) == [error: "the connection is closed"]
      assert true == response_state.done
      assert pid == self()
    end

    test "send a message to parent process to inform the connection is down and reply pending process",
         %{
           state: state,
           request_ref: request_ref,
           stream_response_pid: response_pid
         } do
      socket = state.conn.socket
      # this is a mocked message to inform the connection is closed
      tcp_message = {:tcp_closed, socket}

      response =
        ConnectionProcess.handle_call(
          {:stream_body, request_ref, <<1, 2, 3>>},
          {self(), :tag},
          state
        )

      {:noreply, state, {:continue, :process_request_stream_queue}} = response

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state == :closed
      assert_receive {:elixir_grpc, :connection_down, pid}, 500
      response_state = :sys.get_state(response_pid)
      assert :queue.to_list(response_state.responses) == [error: "the connection is closed"]
      assert true == response_state.done
      assert pid == self()
    end
  end

  describe "handle_info - connection_closed - with dead stream response process" do
    setup :valid_connection
    setup :valid_stream_request
    setup :valid_stream_response

    test "does not crash when ending the stream response process fails",
         %{
           state: state,
           stream_response_pid: response_pid
         } do
      Process.unlink(response_pid)
      monitor = Process.monitor(response_pid)
      Process.exit(response_pid, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^response_pid, :killed}

      socket = state.conn.socket
      tcp_message = {:tcp_closed, socket}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(tcp_message, state)
      assert new_state.conn.state == :closed
      assert_receive {:elixir_grpc, :connection_down, _pid}, 500
    end
  end

  describe "handle_info - response frames for a dead stream response process" do
    setup :valid_connection

    setup do
      test_pid = self()
      handler_id = "test-stream-response-dead-#{inspect(test_pid)}"

      :telemetry.attach(
        handler_id,
        [:grpc, :client, :mint, :stream_response, :dead],
        fn name, measurements, metadata, [] ->
          send(test_pid, {:telemetry, name, measurements, metadata})
        end,
        []
      )

      on_exit(fn -> :telemetry.detach(handler_id) end)
      :ok
    end

    test "cancels the affected request and keeps the connection alive", %{
      process_pid: pid,
      request: {method, path, headers}
    } do
      dead_pid = spawn(fn -> :ok end)
      monitor = Process.monitor(dead_pid)
      assert_receive {:DOWN, ^monitor, :process, ^dead_pid, _reason}

      {:ok, %{request_ref: request_ref}} =
        ConnectionProcess.request(pid, method, path, headers, :stream,
          stream_response_pid: dead_pid
        )

      :ok = ConnectionProcess.stream_request_body(pid, request_ref, :eof)

      assert_receive {:telemetry, [:grpc, :client, :mint, :stream_response, :dead], %{}, metadata},
                     500

      assert metadata.request_ref == request_ref
      assert metadata.stream_response_pid == dead_pid
      assert metadata.reason != nil
      assert Process.alive?(pid)
    end
  end

  describe "retry_timeout/1" do
    test "returns exponentially increasing timeouts" do
      t1 = ConnectionProcess.retry_timeout(1)
      t2 = ConnectionProcess.retry_timeout(2)
      t5 = ConnectionProcess.retry_timeout(5)

      assert t1 >= 800 and t1 <= 1200
      assert t2 > t1
      assert t5 > t2
    end

    test "caps at 120 seconds for attempt >= 11" do
      t11 = ConnectionProcess.retry_timeout(11)
      t15 = ConnectionProcess.retry_timeout(15)

      assert t11 >= 96_000 and t11 <= 144_000
      assert t15 >= 96_000 and t15 <= 144_000
    end
  end

  describe "handle_info :reconnect" do
    setup :valid_connection_with_retry
    setup :attach_reconnect_telemetry

    test "successfully reconnects when server is available", %{
      state: state,
      port: port
    } do
      failed_state = %{state | retry_attempt: 1}

      assert {:noreply, new_state} = ConnectionProcess.handle_info(:reconnect, failed_state)
      assert Mint.HTTP.open?(new_state.conn)
      assert new_state.retry_attempt == 0

      assert_receive {:telemetry, [:grpc, :client, :mint, :reconnect, :stop], measurements,
                      metadata}

      assert is_integer(measurements.duration)
      assert measurements.duration >= 0
      assert metadata.attempt == 2
      assert metadata.max == 3
      assert metadata.host == "127.0.0.1"
      assert metadata.port == port
      assert metadata.scheme == :http
    end
  end

  describe "handle_info :reconnect - unavailable" do
    @describetag private_endpoint: ReconnectUnavailableEndpoint
    setup :start_private_endpoint
    setup :valid_connection_with_retry
    setup :attach_reconnect_telemetry

    test "schedules another reconnect when server is unavailable", %{
      state: state,
      port: port,
      process_pid: pid
    } do
      # Close the live client first so Cowboy shutdown does not wait on drain.
      :ok = ConnectionProcess.disconnect(pid)
      :ok = GRPC.Server.stop_endpoint(ReconnectUnavailableEndpoint)

      failed_state = %{state | retry_attempt: 0, retry_timeout_ms: 10}
      assert {:noreply, new_state} = ConnectionProcess.handle_info(:reconnect, failed_state)
      assert new_state.retry_attempt == 1
      assert_receive :reconnect, 5_000

      assert_receive {:telemetry, [:grpc, :client, :mint, :reconnect, :error], measurements,
                      metadata}

      assert is_integer(measurements.duration)
      assert measurements.duration >= 0
      assert metadata.attempt == 1
      assert metadata.max == 3
      assert metadata.host == "127.0.0.1"
      assert metadata.port == port
      assert metadata.scheme == :http
      assert metadata.reason != nil
    end
  end

  defp start_private_endpoint(%{private_endpoint: endpoint}) do
    {:ok, _, port} = GRPC.Server.start_endpoint(endpoint, 0)

    on_exit(fn ->
      _ = GRPC.Server.stop_endpoint(endpoint)
    end)

    %{port: port}
  end

  defp valid_connection(%{port: port}, opts \\ []) do
    {:ok, pid} =
      ConnectionProcess.start_link(
        :http,
        "127.0.0.1",
        port,
        Keyword.merge([protocols: [:http2]], opts)
      )

    state = :sys.get_state(pid)
    version = Application.spec(:grpc) |> Keyword.get(:vsn)

    headers = [
      {"content-type", "application/grpc"},
      {"user-agent", "grpc-elixir/#{version}"},
      {"te", "trailers"}
    ]

    %{
      process_pid: pid,
      state: state,
      port: port,
      request: {"POST", "/routeguide.RouteGuide/RecordRoute", headers}
    }
  end

  defp valid_stream_request(%{request: {method, path, headers}, process_pid: pid}) do
    {:ok, %{request_ref: request_ref}} =
      ConnectionProcess.request(pid, method, path, headers, :stream, stream_response_pid: self())

    state = :sys.get_state(pid)
    %{request_ref: request_ref, state: state}
  end

  defp valid_stream_response(%{request_ref: request_ref, state: state} = ctx) do
    stream = build(:client_stream)
    {:ok, pid} = StreamResponseProcess.start_link(stream, true)
    state = update_stream_response_process_to_test_pid(state, request_ref, pid)
    Map.merge(ctx, %{state: state, stream_response_pid: pid})
  end

  def update_stream_response_process_to_test_pid(state, request_ref, test_pid) do
    request_ref_state = state.requests[request_ref]

    %{state | requests: %{request_ref => %{request_ref_state | stream_response_pid: test_pid}}}
  end

  defp valid_connection_with_retry(ctx), do: valid_connection(ctx, retry: 3)

  defp attach_reconnect_telemetry(_ctx) do
    test_pid = self()
    handler_id = "test-reconnect-telemetry-#{inspect(test_pid)}"

    :telemetry.attach_many(
      handler_id,
      [
        [:grpc, :client, :mint, :reconnect, :stop],
        [:grpc, :client, :mint, :reconnect, :error],
        [:grpc, :client, :mint, :reconnect, :exhausted]
      ],
      fn name, measurements, metadata, [] ->
        send(test_pid, {:telemetry, name, measurements, metadata})
      end,
      []
    )

    on_exit(fn -> :telemetry.detach(handler_id) end)
    :ok
  end
end

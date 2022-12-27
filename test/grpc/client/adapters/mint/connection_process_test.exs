defmodule GRPC.Client.Adapters.Mint.ConnectionProcessTest do
  use GRPC.DataCase
  alias GRPC.Client.Adapters.Mint.ConnectionProcess
  alias GRPC.Client.Adapters.Mint.StreamResponseProcess

  import ExUnit.CaptureLog

  setup do
    {:ok, _, port} = GRPC.Server.start(FeatureServer, 0)

    on_exit(fn ->
      :ok = GRPC.Server.stop(FeatureServer)
    end)

    %{port: port}
  end

  describe "start_link/4" do
    test "non-successful connection stops the connection process without exit it's caller" do
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
      assert [] == response_state.responses
      assert true == response_state.done
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
      state = %{state | conn: %{state.conn | window_size: 0}}
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
      state = %{state | conn: %{state.conn | window_size: 2}}
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
      assert new_state.conn.window_size == 0
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

      assert [error: %Mint.TransportError{reason: :closed, __exception__: true}] ==
               response_state.responses
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
      assert [error: "the connection is closed"] == response_state.responses
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
      assert [error: "the connection is closed"] == response_state.responses
      assert true == response_state.done
      assert pid == self()
    end
  end

  defp valid_connection(%{port: port}) do
    {:ok, pid} = ConnectionProcess.start_link(:http, "localhost", port, protocols: [:http2])
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
end

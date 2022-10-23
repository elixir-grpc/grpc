defmodule GRPC.Client.Adapters.Mint.StreamResponseProcessTest do
  use GRPC.DataCase
  use ExUnit.Parameterized

  alias GRPC.Client.Adapters.Mint.StreamResponseProcess

  setup do
    state = %{
      buffer: "",
      done: false,
      from: nil,
      grpc_stream: build(:client_stream),
      responses: [],
      compressor: nil,
      send_headers_or_trailers: false
    }

    %{state: state}
  end

  describe "handle_cast/2 - data" do
    setup do
      part_1 = <<0, 0, 0, 0, 12, 10, 10, 72, 101, 108>>
      part_2 = <<108, 111, 32, 76, 117, 105, 115>>
      full_message = part_1 <> part_2
      %{data: {part_1, part_2, full_message}}
    end

    test "append message to buffer when message is incomplete", %{
      state: state,
      data: {part1, _, _}
    } do
      response = StreamResponseProcess.handle_cast({:consume_response, {:data, part1}}, state)

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert new_state.buffer == part1
    end

    test "decode full message when incoming date is complete", %{
      state: state,
      data: {_, _, full_message}
    } do
      expected_response_message = build(:hello_reply_rpc)

      response =
        StreamResponseProcess.handle_cast({:consume_response, {:data, full_message}}, state)

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert new_state.buffer == <<>>
      assert [{:ok, response_message}] = new_state.responses
      assert expected_response_message == response_message
    end

    test "append incoming message to existing buffer", %{state: state, data: {part1, part2, _}} do
      state = %{state | buffer: part1}
      expected_response_message = build(:hello_reply_rpc)
      response = StreamResponseProcess.handle_cast({:consume_response, {:data, part2}}, state)

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert new_state.buffer == <<>>
      assert [{:ok, response_message}] = new_state.responses
      assert expected_response_message == response_message
    end

    test "decode message and put rest on buffer", %{state: state, data: {_, _, full}} do
      extra_data = <<0, 1, 2>>
      data = full <> extra_data
      expected_response_message = build(:hello_reply_rpc)
      response = StreamResponseProcess.handle_cast({:consume_response, {:data, data}}, state)

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert new_state.buffer == extra_data
      assert [{:ok, response_message}] = new_state.responses
      assert expected_response_message == response_message
    end
  end

  describe "handle_cast/2 - headers/trailers" do
    test_with_params(
      "put error in responses when incoming headers has error status",
      %{state: state},
      fn %{type: type, is_header_enabled: header_enabled?} ->
        state = %{state | send_headers_or_trailers: header_enabled?}

        headers = [
          {"content-length", "0"},
          {"content-type", "application/grpc+proto"},
          {"grpc-message", "Internal Server Error"},
          {"grpc-status", "2"},
          {"server", "Cowboy"}
        ]

        response =
          StreamResponseProcess.handle_cast(
            {:consume_response, {type, headers}},
            state
          )

        assert {:noreply, new_state, {:continue, :produce_response}} = response
        assert [{:error, error}] = new_state.responses
        assert %GRPC.RPCError{message: "Internal Server Error", status: 2} == error
      end,
      do: [
        {%{type: :headers, is_header_enabled: false}},
        {%{type: :headers, is_header_enabled: true}},
        {%{type: :trailers, is_header_enabled: true}},
        {%{type: :trailers, is_header_enabled: false}}
      ]
    )

    test_with_params(
      "append headers to response when headers are enabled",
      %{state: state},
      fn type ->
        state = %{state | send_headers_or_trailers: true}

        headers = [
          {"content-length", "0"},
          {"content-type", "application/grpc+proto"},
          {"grpc-message", ""},
          {"grpc-status", "0"},
          {"server", "Cowboy"}
        ]

        response =
          StreamResponseProcess.handle_cast(
            {:consume_response, {type, headers}},
            state
          )

        assert {:noreply, new_state, {:continue, :produce_response}} = response
        assert [{type_response, response_headers}] = new_state.responses
        assert type == type_response

        assert %{
                 "content-length" => "0",
                 "content-type" => "application/grpc+proto",
                 "grpc-message" => "",
                 "grpc-status" => "0",
                 "server" => "Cowboy"
               } == response_headers
      end,
      do: [{:headers}, {:trailers}]
    )

    test_with_params(
      "skip produce headers when flag is disabled and there are no errors",
      %{state: state},
      fn type ->
        headers = [
          {"content-length", "0"},
          {"content-type", "application/grpc+proto"},
          {"grpc-message", ""},
          {"grpc-status", "0"},
          {"server", "Cowboy"}
        ]

        response =
          StreamResponseProcess.handle_cast(
            {:consume_response, {type, headers}},
            state
          )

        assert {:noreply, new_state, {:continue, :produce_response}} = response
        assert [] == new_state.responses
      end,
      do: [{:headers}, {:trailers}]
    )

    test "add compressor to state when incoming headers match available compressor", %{
      state: state
    } do
      headers = [
        {"content-length", "0"},
        {"content-type", "application/grpc+proto"},
        {"grpc-message", ""},
        {"grpc-status", "0"},
        {"server", "Cowboy"},
        {"grpc-encoding", "gzip"}
      ]

      response =
        StreamResponseProcess.handle_cast(
          {:consume_response, {:headers, headers}},
          state
        )

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert GRPC.Compressor.Gzip == new_state.compressor
    end

    test "don't update compressor when unsupported compressor is returned by the server", %{
      state: state
    } do
      headers = [
        {"content-length", "0"},
        {"content-type", "application/grpc+proto"},
        {"grpc-message", ""},
        {"grpc-status", "0"},
        {"server", "Cowboy"},
        {"grpc-encoding", "suzana"}
      ]

      response =
        StreamResponseProcess.handle_cast(
          {:consume_response, {:headers, headers}},
          state
        )

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert nil == new_state.compressor
    end
  end

  describe "handle_cast/2 - errors" do
    test "add error tuple to responses", %{state: state} do
      error = {:error, "howdy"}

      response =
        StreamResponseProcess.handle_cast(
          {:consume_response, error},
          state
        )

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert [response_error] = new_state.responses
      assert response_error == error
    end
  end

  describe "handle_cast/2 - done" do
    test "set state to done", %{state: state} do
      response =
        StreamResponseProcess.handle_cast(
          {:consume_response, :done},
          state
        )

      assert {:noreply, new_state, {:continue, :produce_response}} = response
      assert true == new_state.done
    end
  end

  describe "handle_continue/2 - produce_response" do
    test "noreply when process ref is empty", %{state: state} do
      {:noreply, new_state} = StreamResponseProcess.handle_continue(:produce_response, state)
      assert new_state == state
    end

    test "send nil message to caller process (ends Elixir.Stream) when all responses are sent and stream has ended (done: true)",
         %{state: state} do
      state = %{state | from: {self(), :tag}, done: true}

      {:stop, :normal, _new_state} =
        StreamResponseProcess.handle_continue(:produce_response, state)

      assert_receive {:tag, nil}
    end

    test "continue when there are no response to be sent and stream is not done yet", %{
      state: state
    } do
      state = %{state | from: {self(), :tag}, done: false}
      {:noreply, new_state} = StreamResponseProcess.handle_continue(:produce_response, state)
      assert state == new_state
    end

    test "send response to caller when there are responses in the queue", %{state: state} do
      state = %{state | from: {self(), :tag}, done: false, responses: [1, 2]}
      {:noreply, new_state} = StreamResponseProcess.handle_continue(:produce_response, state)
      %{from: from, responses: responses} = new_state
      assert nil == from
      assert [2] == responses
      assert_receive {:tag, 1}
    end
  end

  describe "build_stream/1" do
    setup do
      {:ok, pid} = StreamResponseProcess.start_link(build(:client_stream), true)

      %{pid: pid}
    end

    test "ends stream when done message is passed", %{pid: pid} do
      stream = StreamResponseProcess.build_stream(pid)
      StreamResponseProcess.done(pid)
      assert Enum.to_list(stream) == []
    end

    test "emits error tuple on stream when error is given to consume", %{pid: pid} do
      stream = StreamResponseProcess.build_stream(pid)
      StreamResponseProcess.consume(pid, :error, "an error")
      StreamResponseProcess.done(pid)
      assert [error] = Enum.to_list(stream)
      assert {:error, "an error"} == error
    end

    test "emits an ok tuple with data", %{pid: pid} do
      data_to_consume = <<0, 0, 0, 0, 12, 10, 10, 72, 101, 108, 108, 111, 32, 76, 117, 105, 115>>
      stream = StreamResponseProcess.build_stream(pid)
      StreamResponseProcess.consume(pid, :data, data_to_consume)
      StreamResponseProcess.done(pid)
      assert [data] = Enum.to_list(stream)
      assert {:ok, build(:hello_reply_rpc)} == data
    end

    test_with_params(
      "emits headers to stream",
      %{pid: pid},
      fn type ->
        headers = [
          {"content-length", "0"},
          {"content-type", "application/grpc+proto"},
          {"grpc-message", ""},
          {"grpc-status", "0"},
          {"server", "Cowboy"}
        ]

        stream = StreamResponseProcess.build_stream(pid)
        StreamResponseProcess.consume(pid, type, headers)
        StreamResponseProcess.done(pid)
        assert [{response_type, response_headers}] = Enum.to_list(stream)
        assert type == response_type

        assert %{
                 "content-length" => "0",
                 "content-type" => "application/grpc+proto",
                 "grpc-message" => "",
                 "grpc-status" => "0",
                 "server" => "Cowboy"
               } == response_headers
      end,
      do: [{:headers}, {:trailers}]
    )
  end
end

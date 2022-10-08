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
  end
end

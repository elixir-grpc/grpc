defmodule GRPC.Integration.MessageLimitEarlyStopTest do
  # Sets the :max_message_length application env, which other tests read.
  use GRPC.Integration.TestCase, async: false

  alias GRPC.Client.Adapters.{Gun, Mint}
  alias GRPC.RPCError

  @limit 1000
  # The prefix claims far more than the limit, but the server sends only the first
  # bytes of the message and then stalls. A client that reads the message to its end
  # waits for bytes that never come, so only a client that stops at the prefix and
  # resets the stream gets an answer, and makes cowboy end the server's request process.
  @claimed 50_000_000

  # Sends `first_chunk`, tells the test which process serves the request, and stalls.
  # Cowboy kills this process when the client resets the stream.
  defmodule StallingReplyHandler do
    def init(req, {test_pid, first_chunk}) do
      {:ok, _request_body, req} = :cowboy_req.read_body(req)
      req = :cowboy_req.stream_reply(200, %{"content-type" => "application/grpc+proto"}, req)
      :cowboy_req.stream_body(first_chunk, :nofin, req)
      send(test_pid, {:handler, self()})

      receive do
        :never -> {:ok, req, first_chunk}
      end
    end
  end

  setup do
    Application.put_env(:grpc, :max_message_length, @limit)
    on_exit(fn -> Application.delete_env(:grpc, :max_message_length) end)
  end

  for adapter <- [Gun, Mint] do
    describe "with the #{inspect(adapter)} adapter" do
      @describetag adapter: adapter

      test "a unary reply over the limit stops at the prefix and resets the stream",
           %{adapter: adapter} do
        port = start_stalling_server(oversize_chunk())
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)

        assert {:error, %RPCError{status: 8, message: message}} =
                 Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "x"},
                   timeout: 5_000
                 )

        assert message =~ "Received message is too large (#{@claimed} bytes"
        assert message =~ "the limit is #{@limit} bytes"
        assert_request_reset()
      end

      test "a streamed reply message over the limit stops at the prefix and resets the stream",
           %{adapter: adapter} do
        feature = Protobuf.encode(%Routeguide.Feature{name: "within the limit"})
        small = <<0, byte_size(feature)::32, feature::binary>>
        port = start_stalling_server(small <> oversize_chunk())
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)

        {:ok, replies} =
          Routeguide.RouteGuide.Stub.list_features(channel, %Routeguide.Rectangle{})

        items = Enum.to_list(replies)

        assert [{:ok, %Routeguide.Feature{name: "within the limit"}} | _] = items
        assert {:error, %RPCError{status: 8, message: message}} = List.last(items)
        assert message =~ "the limit is #{@limit} bytes"
        assert_request_reset()
      end
    end
  end

  # The 5-byte prefix of a message of `@claimed` bytes, and 100 bytes of it.
  defp oversize_chunk, do: <<0, @claimed::32, String.duplicate("r", 100)::binary>>

  # The server's request process ends once the client resets the stream. This waits
  # for that, with no sleep; a client that never resets leaves it alive.
  defp assert_request_reset do
    assert_receive {:handler, handler}, 5_000
    ref = Process.monitor(handler)
    assert_receive {:DOWN, ^ref, :process, ^handler, _reason}, 5_000
  end

  defp start_stalling_server(first_chunk) do
    ref = {__MODULE__, make_ref()}
    state = {self(), first_chunk}
    dispatch = :cowboy_router.compile([{:_, [{:_, StallingReplyHandler, state}]}])
    {:ok, _} = :cowboy.start_clear(ref, [port: 0], %{env: %{dispatch: dispatch}})
    on_exit(fn -> :cowboy.stop_listener(ref) end)
    :ranch.get_port(ref)
  end
end

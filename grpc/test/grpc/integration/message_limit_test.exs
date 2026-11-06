defmodule GRPC.Integration.MessageLimitTest do
  # Sets the :max_message_length application env, which other tests read.
  use GRPC.Integration.TestCase, async: false

  alias GRPC.Client.Adapters.{Gun, Mint}
  alias GRPC.RPCError

  @limit 1000

  # Receiving is enforced by `GRPC.Message`, in grpc_core. This package resolves
  # grpc_core from hex, so these tests run once the version it requires has the check.
  @core_enforces_receive Code.ensure_loaded?(GRPC.Message) and
                           function_exported?(GRPC.Message, :max_message_length, 0)

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream), do: %Helloworld.HelloReply{message: "Hello, #{req.name}"}
  end

  defmodule RouteServer do
    use GRPC.Server, service: Routeguide.RouteGuide.Service

    def route_chat(req_enum, stream) do
      Enum.each(req_enum, fn note -> GRPC.Server.send_reply(stream, note) end)
    end
  end

  # Answers every request with the same, already framed, gRPC body, so a test can
  # send the client a message that the server side would refuse to send.
  defmodule FixedReplyHandler do
    def init(req, body) do
      {:ok, _request_body, req} = :cowboy_req.read_body(req)
      req = :cowboy_req.stream_reply(200, %{"content-type" => "application/grpc+proto"}, req)
      :cowboy_req.stream_body(body, :nofin, req)
      :cowboy_req.stream_trailers(%{"grpc-status" => "0"}, req)
      {:ok, req, body}
    end
  end

  setup do
    Application.put_env(:grpc, :max_message_length, @limit)
    on_exit(fn -> Application.delete_env(:grpc, :max_message_length) end)
  end

  for adapter <- [Gun, Mint] do
    describe "with the #{inspect(adapter)} adapter" do
      @describetag adapter: adapter

      test "a unary request over the limit fails with RESOURCE_EXHAUSTED before it is sent",
           %{adapter: adapter} do
        run_server(HelloServer, fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)
          request = %Helloworld.HelloRequest{name: String.duplicate("x", @limit + 100)}

          assert {:error, %RPCError{status: 8, message: message}} =
                   Helloworld.Greeter.Stub.say_hello(channel, request)

          assert message =~ "Message to send is too large"
          assert message =~ "the limit is #{@limit} bytes"
        end)
      end

      test "a unary request under the limit succeeds", %{adapter: adapter} do
        run_server(HelloServer, fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)
          request = %Helloworld.HelloRequest{name: String.duplicate("x", @limit - 100)}

          assert {:ok, %Helloworld.HelloReply{}} =
                   Helloworld.Greeter.Stub.say_hello(channel, request)
        end)
      end

      test "a streamed request over the limit raises RESOURCE_EXHAUSTED, a smaller one is sent",
           %{adapter: adapter} do
        run_server(RouteServer, fn port ->
          {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)
          stream = Routeguide.RouteGuide.Stub.route_chat(channel)

          small = %Routeguide.RouteNote{message: "small"}
          assert %GRPC.Client.Stream{} = GRPC.Stub.send_request(stream, small)

          big = %Routeguide.RouteNote{message: String.duplicate("x", @limit + 100)}

          error = assert_raise RPCError, fn -> GRPC.Stub.send_request(stream, big) end
          assert error.status == 8
          assert error.message =~ "the limit is #{@limit} bytes"

          GRPC.Stub.cancel(stream)
        end)
      end

      @tag skip: not @core_enforces_receive
      test "a unary reply over the limit fails with RESOURCE_EXHAUSTED", %{adapter: adapter} do
        port =
          start_fixed_reply_server(
            <<0, @limit + 100::32, String.duplicate("r", @limit + 100)::binary>>
          )

        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)

        assert {:error, %RPCError{status: 8, message: message}} =
                 Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "x"})

        assert message =~ "Received message is too large"
        assert message =~ "the limit is #{@limit} bytes"
      end

      # The Gun adapter raises while the stream is read, the Mint adapter returns the
      # error as an item.
      @tag skip: not @core_enforces_receive
      test "a streamed reply message over the limit fails with RESOURCE_EXHAUSTED",
           %{adapter: adapter} do
        feature = Protobuf.encode(%Routeguide.Feature{name: "within the limit"})
        small = <<0, byte_size(feature)::32, feature::binary>>
        big = <<0, @limit + 100::32, String.duplicate("r", @limit + 100)::binary>>
        port = start_fixed_reply_server(small <> big)
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)

        {:ok, replies} =
          Routeguide.RouteGuide.Stub.list_features(channel, %Routeguide.Rectangle{})

        items =
          try do
            Enum.to_list(replies)
          rescue
            error in RPCError -> [{:error, error}]
          end

        assert {:error, %RPCError{status: 8, message: message}} = List.last(items)
        assert message =~ "the limit is #{@limit} bytes"
      end

      test "a unary reply under the limit succeeds", %{adapter: adapter} do
        reply = Protobuf.encode(%Helloworld.HelloReply{message: "within the limit"})
        port = start_fixed_reply_server(<<0, byte_size(reply)::32, reply::binary>>)
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter: adapter)

        assert {:ok, %Helloworld.HelloReply{message: "within the limit"}} =
                 Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "x"})
      end
    end
  end

  defp start_fixed_reply_server(body) do
    ref = {__MODULE__, make_ref()}
    dispatch = :cowboy_router.compile([{:_, [{:_, FixedReplyHandler, body}]}])
    {:ok, _} = :cowboy.start_clear(ref, [port: 0], %{env: %{dispatch: dispatch}})
    on_exit(fn -> :cowboy.stop_listener(ref) end)
    :ranch.get_port(ref)
  end
end

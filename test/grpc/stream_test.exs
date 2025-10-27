defmodule GRPC.StreamTest do
  use GRPC.Integration.TestCase
  doctest GRPC.Stream

  describe "simple test" do
    defmodule TestInput do
      defstruct [:message]
    end

    defmodule FakeAdapter do
      def get_headers(_), do: %{"content-type" => "application/grpc"}

      def send_reply(%{test_pid: test_pid, ref: ref}, item, _opts) do
        send(test_pid, {:send_reply, ref, item})
      end

      def send_trailers(%{test_pid: test_pid, ref: ref}, trailers) do
        send(test_pid, {:send_trailers, ref, trailers})
      end
    end

    test "unary/2 creates a flow from a unary input" do
      test_pid = self()
      ref = make_ref()

      input = %Routeguide.Point{latitude: 1, longitude: 2}

      materializer = %GRPC.Server.Stream{
        adapter: FakeAdapter,
        payload: %{test_pid: test_pid, ref: ref},
        grpc_type: :unary
      }

      assert :noreply =
               GRPC.Stream.unary(input, materializer: materializer)
               |> GRPC.Stream.map(fn item ->
                 item
               end)
               |> GRPC.Stream.run()

      assert_receive {:send_reply, ^ref, response}
      assert IO.iodata_to_binary(response) == Protobuf.encode(input)
    end

    test "unary/2 creates a flow with metadata" do
      input = %TestInput{message: 1}
      materializer = %GRPC.Server.Stream{adapter: FakeAdapter, grpc_type: :unary}

      flow =
        GRPC.Stream.unary(input, materializer: materializer, propagate_context: true)
        |> GRPC.Stream.map_with_context(fn meta, item ->
          assert not is_nil(meta)
          assert is_map(meta)
          item
        end)

      result = Enum.to_list(GRPC.Stream.to_flow(flow)) |> Enum.at(0)
      assert result == input
    end

    test "from/2 creates a flow from enumerable input" do
      input = [%{message: "a"}, %{message: "b"}]

      flow =
        GRPC.Stream.from(input, max_demand: 1)
        |> GRPC.Stream.map(& &1)

      result = Enum.to_list(GRPC.Stream.to_flow(flow))
      assert result == input
    end

    test "from_as_ctx/3 creates a flow from enumerable input" do
      input = [%{message: "a"}, %{message: "b"}]
      materializer = %GRPC.Server.Stream{adapter: FakeAdapter, grpc_type: :unary}

      flow =
        GRPC.Stream.from(input, propagate_context: true, materializer: materializer)
        |> GRPC.Stream.map_with_context(fn meta, item ->
          assert not is_nil(meta)
          assert is_map(meta)
          item
        end)

      result = Enum.to_list(GRPC.Stream.to_flow(flow))
      assert result == input
    end
  end

  describe "from/2" do
    test "converts a list into a flow" do
      stream = GRPC.Stream.from([1, 2, 3])
      assert %GRPC.Stream{} = stream

      result = stream |> GRPC.Stream.map(&(&1 * 2)) |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert Enum.sort(result) == [2, 4, 6]
    end
  end

  describe "ask/3 with pid" do
    test "calls a pid and returns the response" do
      pid =
        spawn(fn ->
          receive do
            {:request, :hello, test_pid} ->
              send(test_pid, {:response, :world})
          end
        end)

      result =
        GRPC.Stream.from([:hello])
        |> GRPC.Stream.ask(pid)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert result == [:world]
    end

    test "returns error if pid not alive" do
      pid = spawn(fn -> :ok end)
      # wait for the process to exit
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, _, _, _}

      result =
        GRPC.Stream.from(["msg"])
        |> GRPC.Stream.ask(pid)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert result == [{:error, :process_not_alive}]
    end
  end

  describe "ask/3 with GenServer" do
    defmodule TestServer do
      use GenServer

      def start_link(_) do
        GenServer.start_link(__MODULE__, nil, name: __MODULE__)
      end

      def init(_), do: {:ok, %{}}

      def handle_info({:request, value, from}, state) do
        Process.send(from, {:response, value}, [])
        {:noreply, state}
      end
    end

    setup do
      {:ok, _pid} = TestServer.start_link([])
      :ok
    end

    test "asks GenServer and receives correct response" do
      stream = GRPC.Stream.from(["abc"])

      result =
        stream
        |> GRPC.Stream.ask(TestServer)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert result == ["abc"]
    end
  end

  describe "ask/3 error handling" do
    test "returns timeout error if response not received in time" do
      pid =
        spawn_link(fn ->
          Process.sleep(:infinity)
        end)

      result =
        GRPC.Stream.from([:hello])
        # very short timeout
        |> GRPC.Stream.ask(pid, 10)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert result == [{:error, :timeout}]
    end
  end

  describe "safe_invoke/2 handling {:ok, value} and direct value" do
    test "maps {:ok, value} to value" do
      stream =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.map(fn x -> {:ok, x * 10} end)

      result = stream |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert result == [10, 20]
    end

    test "keeps direct values as is" do
      stream =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.map(fn x -> x * 5 end)

      result = stream |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert result == [5, 10]
    end
  end

  describe "safe_invoke/2 catches errors" do
    test "map/2 handles function returning {:error, reason}" do
      stream =
        GRPC.Stream.from([1, 2, 3])
        |> GRPC.Stream.map(fn
          2 -> {:error, :fail}
          x -> x
        end)

      results = stream |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert results == [1, {:error, :fail}, 3]
    end

    test "map/2 catches exceptions" do
      stream =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.map(fn
          2 -> raise "boom"
          x -> x
        end)

      results = stream |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert match?([1, {:error, {:exception, %RuntimeError{message: "boom"}}}], results)
    end

    test "flat_map/2 catches thrown values" do
      stream =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.flat_map(fn
          2 -> throw(:fail)
          x -> [x]
        end)

      results = stream |> GRPC.Stream.to_flow() |> Enum.to_list()
      assert results == [1, {:error, {:throw, :fail}}]
    end
  end

  describe "map_error/2" do
    test "transforms {:error, reason} tuples" do
      stream =
        GRPC.Stream.from([{:error, :invalid_input}, {:ok, 42}, 100])
        |> GRPC.Stream.map_error(fn
          {:error, :invalid_input} -> {:error, :mapped_error}
          msg -> msg
        end)

      result = stream |> GRPC.Stream.to_flow() |> Enum.to_list()

      assert Enum.sort(result) == [
               42,
               100,
               {:error,
                %GRPC.RPCError{
                  __exception__: true,
                  details: nil,
                  message: ":mapped_error",
                  status: nil
                }}
             ]
    end

    test "transforms exceptions raised inside previous map" do
      stream =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.map(fn
          2 -> raise "boom"
          x -> x
        end)
        |> GRPC.Stream.map_error(fn
          {:error, %RuntimeError{message: "boom"}} ->
            GRPC.RPCError.exception(message: "Validation or runtime error")

          msg ->
            msg
        end)

      result = stream |> GRPC.Stream.to_flow() |> Enum.to_list()

      assert match?(
               [
                 1,
                 {:error,
                  %GRPC.RPCError{
                    status: nil,
                    message: "{:exception, %RuntimeError{message: \"boom\"}}",
                    details: nil
                  }}
               ],
               result
             )
    end
  end

  describe "map/2, flat_map/2, filter/2" do
    test "maps values correctly" do
      result =
        GRPC.Stream.from([1, 2, 3])
        |> GRPC.Stream.map(&(&1 * 10))
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert Enum.sort(result) == [10, 20, 30]
    end

    test "flat_maps values correctly" do
      result =
        GRPC.Stream.from([1, 2])
        |> GRPC.Stream.flat_map(&[&1, &1])
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert Enum.sort(result) == [1, 1, 2, 2]
    end

    test "filters values correctly" do
      result =
        GRPC.Stream.from([1, 2, 3, 4])
        |> GRPC.Stream.filter(&(rem(&1, 2) == 0))
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert result == [2, 4]
    end
  end

  describe "effect/2" do
    test "applies side effects without altering values" do
      parent = self()

      result =
        GRPC.Stream.from([1, 2, 3])
        |> GRPC.Stream.effect(fn x -> send(parent, {:effect_called, x}) end)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      assert Enum.sort(result) == [1, 2, 3]

      assert_receive {:effect_called, 1}
      assert_receive {:effect_called, 2}
      assert_receive {:effect_called, 3}
    end

    test "continues pipeline even if effect function raises an error" do
      parent = self()

      result =
        GRPC.Stream.from([1, 2, 3])
        |> GRPC.Stream.effect(fn
          2 -> raise "boom"
          x -> send(parent, {:effect_called, x})
        end)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()

      # Even with error 2, the pipeline should continue and return all elements
      assert Enum.sort(result) == [1, 2, 3]

      # The effect should have been called for 1 and 3
      assert_receive {:effect_called, 1}
      assert_receive {:effect_called, 3}
    end
  end

  describe "test complex operations" do
    test "pipeline with all GRPC.Stream operators" do
      target =
        spawn(fn ->
          receive_loop()
        end)

      input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      result =
        input
        |> GRPC.Stream.from()
        # 2..11
        |> GRPC.Stream.map(&(&1 + 1))
        # [2,4,3,6,4,8,...]
        |> GRPC.Stream.flat_map(&[&1, &1 * 2])
        # keep evens
        |> GRPC.Stream.filter(&(rem(&1, 2) == 0))

        # remove duplicates
        |> GRPC.Stream.uniq()
        # multiply by 10 via process
        |> GRPC.Stream.ask(target)
        |> GRPC.Stream.partition()
        |> GRPC.Stream.reduce(fn -> [] end, fn i, acc -> [i | acc] end)
        |> GRPC.Stream.to_flow()
        |> Enum.to_list()
        |> List.flatten()
        |> Enum.sort()

      assert result == [20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220]
    end
  end

  describe "join_with/merge streams" do
    test "merges input stream with joined GenStage producer" do
      defmodule TestProducer do
        use GenStage

        def start_link(items) do
          GenStage.start_link(__MODULE__, items)
        end

        def init(items) do
          {:producer, items}
        end

        def handle_demand(demand, state) when demand > 0 do
          {events, remaining} = Enum.split(state, demand)

          {:noreply, events, remaining}
        end
      end

      elements = Enum.to_list(4..1000)
      {:ok, producer_pid} = TestProducer.start_link(elements)

      input = [1, 2, 3]

      task =
        Task.async(fn ->
          GRPC.Stream.from(input, join_with: producer_pid, max_demand: 500)
          |> GRPC.Stream.map(fn it -> it end)
          |> GRPC.Stream.run_with(%GRPC.Server.Stream{}, dry_run: true)
        end)

      result =
        case Task.yield(task, 1000) || Task.shutdown(task) do
          {:ok, _} -> :ok
          _ -> :ok
        end

      if Process.alive?(producer_pid) do
        Process.exit(producer_pid, :normal)
      end

      assert result == :ok
    end
  end

  describe "run/1" do
    defmodule MyGRPCService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
        end)
        |> GRPC.Stream.run()
      end
    end

    test "runs a unary stream" do
      run_server([MyGRPCService], fn port ->
        point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        expected_response = %Routeguide.Feature{
          location: point,
          name: "#{point.latitude},#{point.longitude}"
        }

        assert {:ok, response, %{trailers: trailers}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, point, return_headers: true)

        assert response == expected_response
        assert trailers == GRPC.Transport.HTTP2.server_trailers()
      end)
    end
  end

  defp receive_loop do
    receive do
      {:request, item, from} ->
        send(from, {:response, item * 10})
        receive_loop()
    end
  end
end

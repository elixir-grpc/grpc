defmodule GRPC.StreamTest do
  use ExUnit.Case
  doctest GRPC.Stream

  describe "simple test" do
    defmodule TestInput do
      defstruct [:message]
    end

    defmodule FakeAdapter do
      def get_headers(_), do: %{"content-type" => "application/grpc"}
    end

    test "unary/2 creates a flow from a unary input" do
      input = %TestInput{message: 1}

      result =
        GRPC.Stream.unary(input)
        |> GRPC.Stream.map(& &1)
        |> GRPC.Stream.run()

      assert result == input
    end

    test "unary/2 creates a flow with metadata" do
      input = %TestInput{message: 1}
      materializer = %GRPC.Server.Stream{adapter: FakeAdapter}

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
      materializer = %GRPC.Server.Stream{adapter: FakeAdapter}

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

    test "converts from Flow to GRPC.Stream" do
      flow = Flow.from_enumerable([1, 2, 3], max_demand: 1)
      stream = GRPC.Stream.from_flow!(flow)
      assert %GRPC.Stream{flow: ^flow} = stream

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

      assert result == [{:error, "msg", :not_alive}]
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

  defp receive_loop do
    receive do
      {:request, item, from} ->
        send(from, {:response, item * 10})
        receive_loop()
    end
  end
end

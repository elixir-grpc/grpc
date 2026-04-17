defmodule GRPC.Integration.PoolTest do
  use GRPC.Integration.TestCase

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  defmodule SlowServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      Process.sleep(100)
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end

    def loop_back(req, _stream) do
      client_pid = :erlang.binary_to_term(req.client_pid)
      client_msg = :erlang.binary_to_term(req.client_msg)
      send(client_pid, client_msg)
      Process.sleep(100)
      %Helloworld.LoopBackReply{message: "Hello, #{req.client_name}"}
    end
  end

  defmodule ErrorServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(_req, _stream) do
      raise GRPC.RPCError, status: :internal, message: "forced error"
    end
  end

  test "RPC calls work through the pool" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      assert {:ok, %Helloworld.HelloReply{message: "Hello, World"}} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "World"})
    end)
  end

  test "pool_size controls the number of open channels" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", pool: %{size: 3})

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, channel.pool})

      state = :sys.get_state(pool_pid)
      assert map_size(state.channels) == 3
    end)
  end

  test "concurrent requests from multiple processes all succeed" do
    run_server(SlowServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")

      tasks =
        for i <- 1..20 do
          Task.async(fn ->
            Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "#{i}"})
          end)
        end

      results = Task.await_many(tasks, 5_000)
      assert Enum.all?(results, fn {status, _} -> status == :ok end)
    end)
  end

  test "channel is returned to pool after use, allowing sequential reuse" do
    run_server(HelloServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 0, max_streams: 1})

      assert {:ok, _} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "first"})

      assert {:ok, _} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "second"})
    end)
  end

  test "overflow channel is opened when all pool channels are at max streams" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 1, max_streams: 1})

      client_pid = self()
      client_msg = :received_loopback_request

      task1 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary({client_msg, 1}),
            client_name: "1"
          })
        end)

      assert_receive {:received_loopback_request, 1}

      task2 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary({client_msg, 2}),
            client_name: "2"
          })
        end)

      assert_receive {:received_loopback_request, 2}

      assert {:ok, %Helloworld.LoopBackReply{}} = Task.await(task1, 2_000)
      assert {:ok, %Helloworld.LoopBackReply{}} = Task.await(task2, 2_000)
    end)
  end

  test "lease is released automatically when caller process dies mid-RPC" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_streams: 1})

      pool_ref = channel.pool

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

      test_pid = self()

      task =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(test_pid),
            client_msg: :erlang.term_to_binary(:acquired),
            client_name: "crash"
          })
        end)

      # Server sends :acquired once it has received the request (channel is held by task)
      assert_receive :acquired
      Task.shutdown(task, :brutal_kill)

      # :sys.get_state is a synchronous GenServer call — it sits behind the :DOWN message
      # in the pool server's mailbox, so by the time it returns the lease is already cleared
      state = :sys.get_state(pool_pid)
      assert Enum.all?(state.channels, fn {_, ch} -> ch.open_streams == 0 end)
      assert map_size(state.leases_by_monitor_ref) == 0

      assert {:ok, _} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "after"})
    end)
  end

  test "pool replaces a crashed channel immediately to maintain pool_size" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", pool: %{size: 2})
      pool_ref = channel.pool

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

      %{channels: channels} = state = :sys.get_state(pool_pid)
      old_references = Enum.map(channels, fn {id, _} -> id end)

      [{_, wrapped_ch} | _] = Map.to_list(state.channels)
      conn_pid = wrapped_ch.channel.adapter_payload.conn_pid

      Process.exit(conn_pid, :kill)

      # :sys.get_state sits behind the :EXIT in the pool server's mailbox, so by the
      # time it returns the crash has been handled and the replacement channel opened
      spawn(fn ->
        %{channels: channels} = :sys.get_state(pool_pid)
        new_references = Enum.map(channels, fn {id, _} -> id end)

        assert Enum.count(old_references) == 2
        assert Enum.count(new_references) == 2

        assert (new_references ++ old_references) |> Enum.uniq() |> Enum.count() == 3
      end)

      Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "after"})
    end)
  end

  test "nil max_overflow allows unbounded overflow connections" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: nil, max_streams: 1})

      pool_ref = channel.pool

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

      client_pid = self()

      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
              client_pid: :erlang.term_to_binary(client_pid),
              client_msg: :erlang.term_to_binary({:holding, i}),
              client_name: "#{i}"
            })
          end)
        end

      for i <- 1..5, do: assert_receive({:holding, ^i})

      state = :sys.get_state(pool_pid)
      assert map_size(state.channels) == 5

      for task <- tasks, do: assert({:ok, _} = Task.await(task, 2_000))
    end)
  end

  test "overflow channel increases channel count in pool state" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 1, max_streams: 1})

      pool_ref = channel.pool

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

      client_pid = self()

      task1 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary(:holding_1),
            client_name: "1"
          })
        end)

      assert_receive :holding_1

      task2 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary(:holding_2),
            client_name: "2"
          })
        end)

      assert_receive :holding_2

      # :sys.get_state is synchronous — by the time it returns the overflow channel is open
      state = :sys.get_state(pool_pid)
      assert map_size(state.channels) == 2

      assert {:ok, _} = Task.await(task1, 2_000)
      assert {:ok, _} = Task.await(task2, 2_000)
    end)
  end

  test "resource_exhausted when pool is full and max_overflow is 0" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 0, max_streams: 1})

      client_pid = self()

      task =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary(:holding),
            client_name: "1"
          })
        end)

      assert_receive :holding

      assert {:error, %GRPC.RPCError{status: status}} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "blocked"})

      assert status == GRPC.Status.resource_exhausted()

      Task.await(task, 2_000)
    end)
  end

  test "resource_exhausted when pool and all overflow channels are at max_streams" do
    run_server(SlowServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_overflow: 1, max_streams: 1})

      client_pid = self()

      task1 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary(:holding_1),
            client_name: "1"
          })
        end)

      assert_receive :holding_1

      task2 =
        Task.async(fn ->
          Helloworld.Greeter.Stub.loop_back(channel, %Helloworld.LoopBackRequest{
            client_pid: :erlang.term_to_binary(client_pid),
            client_msg: :erlang.term_to_binary(:holding_2),
            client_name: "2"
          })
        end)

      assert_receive :holding_2

      assert {:error, %GRPC.RPCError{status: status}} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "blocked"})

      assert status == GRPC.Status.resource_exhausted()

      assert {:ok, _} = Task.await(task1, 2_000)
      assert {:ok, _} = Task.await(task2, 2_000)
    end)
  end

  test "channel is returned to pool even when RPC returns an error" do
    run_server(ErrorServer, fn port ->
      {:ok, channel} =
        GRPC.Stub.connect("localhost:#{port}", pool: %{size: 1, max_streams: 1})

      pool_ref = channel.pool

      [{pool_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Server, pool_ref})

      assert {:error, _} =
               Helloworld.Greeter.Stub.say_hello(channel, %Helloworld.HelloRequest{name: "fail"})

      # checkin is a cast — :sys.get_state sits behind it in the mailbox
      state = :sys.get_state(pool_pid)
      assert Enum.all?(state.channels, fn {_, ch} -> ch.open_streams == 0 end)
      assert map_size(state.leases_by_monitor_ref) == 0
    end)
  end

  test "disconnect stops the pool supervisor" do
    run_server(HelloServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      pool_ref = channel.pool

      [{sup_pid, _}] =
        Registry.lookup(GRPC.Client.Pool.Registry, {GRPC.Client.Pool.Supervisor, pool_ref})

      assert Process.alive?(sup_pid)

      {:ok, disconnected} = GRPC.Stub.disconnect(channel)
      assert disconnected.pool == nil

      refute Process.alive?(sup_pid)
    end)
  end
end

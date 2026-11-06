defmodule GRPC.Client.Adapters.Gun.StreamResponseProcessTest do
  use ExUnit.Case, async: true

  alias GRPC.Client.Adapters.Gun.StreamResponseProcess

  test "stops after a terminal response is consumed" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:gun_response, self(), make_ref(), :fin, 200, []})

    assert {:response, :fin, 200, []} = StreamResponseProcess.await(pid, deadline_after(100))
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "returns an error for unexpected messages instead of timing out" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, :unexpected_message)

    assert {:error, {:unexpected_message, inspected_message}} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert inspected_message == ":unexpected_message"
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "maps connection-level gun errors to connection errors" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    reason = {:protocol_error, :"The preface was not received in a reasonable amount of time."}
    send(pid, {:gun_error, self(), reason})

    assert {:error, {:connection_error, ^reason}} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "returns connection-level gun errors to an awaiting caller" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    task = Task.async(fn -> StreamResponseProcess.await(pid, deadline_after(1_000)) end)
    assert_waiter(pid)

    reason = {:protocol_error, :"The preface was not received in a reasonable amount of time."}
    send(pid, {:gun_error, self(), reason})

    assert {:error, {:connection_error, ^reason}} = Task.await(task)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "keeps queued shutdown errors after non-final headers" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:gun_response, self(), make_ref(), :nofin, 200, []})
    send(pid, {:connection_down, :shutdown})

    assert {:response, :nofin, 200, []} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert {:error, {:connection_error, :shutdown}} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500

    assert {:error, {:connection_error, :closed}} =
             StreamResponseProcess.await(pid, deadline_after(100))
  end

  test "ignores connection errors after terminal trailers are already queued" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:gun_trailers, self(), make_ref(), [{"grpc-status", "0"}]})
    send(pid, {:connection_down, :shutdown})

    assert {:trailers, [{"grpc-status", "0"}]} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "ignores duplicate terminal connection errors" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:connection_down, :first})
    send(pid, {:connection_down, :second})

    assert {:error, {:connection_error, :first}} =
             StreamResponseProcess.await(pid, deadline_after(100))

    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "uses one deadline across response phases" do
    {:ok, pid} = StreamResponseProcess.start_link()
    stream_ref = make_ref()
    deadline = deadline_after(1_000)
    :ok = StreamResponseProcess.track(pid, self(), stream_ref, deadline)
    timer_ref = :sys.get_state(pid).deadline_timer

    send(pid, {:gun_response, self(), stream_ref, :nofin, 200, []})
    assert {:response, :nofin, 200, []} = StreamResponseProcess.await(pid, deadline)
    assert :sys.get_state(pid).deadline_timer == timer_ref

    task = Task.async(fn -> StreamResponseProcess.await(pid, deadline) end)
    assert_waiter(pid)
    send(pid, {:deadline_expired, timer_ref})

    assert {:error, :timeout} = Task.await(task)
    assert_receive {:stream_expired, ^stream_ref, ^pid}
  end

  test "terminal response wins when it is handled before expiry" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)
    stream_ref = make_ref()
    deadline = deadline_after(1_000)
    :ok = StreamResponseProcess.track(pid, self(), stream_ref, deadline)
    timer_ref = :sys.get_state(pid).deadline_timer

    task = Task.async(fn -> StreamResponseProcess.await(pid, deadline) end)
    assert_waiter(pid)

    send(pid, {:gun_response, self(), stream_ref, :fin, 200, []})
    send(pid, {:deadline_expired, timer_ref})

    assert {:response, :fin, 200, []} = Task.await(task)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
    refute_receive {:stream_expired, ^stream_ref, ^pid}
  end

  test "expiry wins over terminal and late messages" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)
    stream_ref = make_ref()
    deadline = deadline_after(1_000)
    :ok = StreamResponseProcess.track(pid, self(), stream_ref, deadline)
    timer_ref = :sys.get_state(pid).deadline_timer

    task = Task.async(fn -> StreamResponseProcess.await(pid, deadline) end)
    assert_waiter(pid)

    send(pid, {:deadline_expired, timer_ref})
    send(pid, {:gun_response, self(), stream_ref, :fin, 200, []})
    send(pid, {:gun_data, self(), stream_ref, :fin, "late"})

    assert {:error, :timeout} = Task.await(task)
    assert_receive {:stream_expired, ^stream_ref, ^pid}
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
    refute Process.alive?(pid)
  end

  describe "flow credit" do
    # The test process stands in for Gun, so `:gun.update_flow/3` casts to it.
    test "credits one frame to Gun when a queued data message is taken" do
      {:ok, pid} = StreamResponseProcess.start_link()
      stream_ref = make_ref()

      send(pid, {:gun_data, self(), stream_ref, :nofin, "one"})
      send(pid, {:gun_data, self(), stream_ref, :nofin, "two"})
      assert_queued(pid, 2)
      refute_receive {:"$gen_cast", {:update_flow, _, _, _}}, 20

      assert {:data, :nofin, "one"} = StreamResponseProcess.await(pid, deadline_after(5_000))
      assert_receive {:"$gen_cast", {:update_flow, ^pid, ^stream_ref, 1}}, 1_000
      refute_receive {:"$gen_cast", {:update_flow, _, _, _}}, 20

      assert {:data, :nofin, "two"} = StreamResponseProcess.await(pid, deadline_after(5_000))
      assert_receive {:"$gen_cast", {:update_flow, ^pid, ^stream_ref, 1}}, 1_000
    end

    test "credits a frame handed straight to a waiting caller" do
      {:ok, pid} = StreamResponseProcess.start_link()
      stream_ref = make_ref()

      task = Task.async(fn -> StreamResponseProcess.await(pid, deadline_after(1_000)) end)
      assert_waiter(pid)
      send(pid, {:gun_data, self(), stream_ref, :nofin, "one"})

      assert {:data, :nofin, "one"} = Task.await(task)
      assert_receive {:"$gen_cast", {:update_flow, ^pid, ^stream_ref, 1}}, 1_000
    end

    test "credits nothing for the last frame or once the stream is done" do
      {:ok, pid} = StreamResponseProcess.start_link()
      stream_ref = make_ref()

      send(pid, {:gun_data, self(), stream_ref, :nofin, "one"})
      send(pid, {:gun_trailers, self(), stream_ref, [{"grpc-status", "0"}]})
      assert_queued(pid, 2)

      assert {:data, :nofin, "one"} = StreamResponseProcess.await(pid, deadline_after(5_000))
      assert {:trailers, _} = StreamResponseProcess.await(pid, deadline_after(5_000))
      refute_receive {:"$gen_cast", {:update_flow, _, _, _}}, 20

      {:ok, pid} = StreamResponseProcess.start_link()
      send(pid, {:gun_data, self(), stream_ref, :fin, "last"})
      assert {:data, :fin, "last"} = StreamResponseProcess.await(pid, deadline_after(5_000))
      refute_receive {:"$gen_cast", {:update_flow, _, _, _}}, 20
    end
  end

  defp assert_queued(pid, count, attempts \\ 50)

  defp assert_queued(_pid, count, 0), do: flunk("expected #{count} queued messages")

  defp assert_queued(pid, count, attempts) do
    if :queue.len(:sys.get_state(pid).messages) == count do
      :ok
    else
      Process.sleep(1)
      assert_queued(pid, count, attempts - 1)
    end
  end

  defp deadline_after(milliseconds) do
    System.monotonic_time(:millisecond) + milliseconds
  end

  defp assert_waiter(pid, attempts \\ 50)

  defp assert_waiter(_pid, 0), do: flunk("response process did not install a waiter")

  defp assert_waiter(pid, attempts) do
    if :sys.get_state(pid).waiter do
      :ok
    else
      Process.sleep(1)
      assert_waiter(pid, attempts - 1)
    end
  end
end

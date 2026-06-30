defmodule GRPC.Client.Adapters.Gun.StreamResponseProcessTest do
  use ExUnit.Case, async: true

  alias GRPC.Client.Adapters.Gun.StreamResponseProcess

  test "stops after a terminal response is consumed" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:gun_response, self(), make_ref(), :fin, 200, []})

    assert {:response, :fin, 200, []} = StreamResponseProcess.await(pid, 100)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "returns an error for unexpected messages instead of timing out" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, :unexpected_message)

    assert {:error, {:unexpected_message, inspected_message}} =
             StreamResponseProcess.await(pid, 100)

    assert inspected_message == ":unexpected_message"
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "maps connection-level gun errors to connection errors" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    reason = {:protocol_error, :"The preface was not received in a reasonable amount of time."}
    send(pid, {:gun_error, self(), reason})

    assert {:error, {:connection_error, ^reason}} = StreamResponseProcess.await(pid, 100)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "returns connection-level gun errors to an awaiting caller" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    task = Task.async(fn -> StreamResponseProcess.await(pid, 1_000) end)

    waiter =
      Enum.find_value(1..50, fn _ ->
        case :sys.get_state(pid) do
          %{waiter: nil} ->
            Process.sleep(10)
            nil

          %{waiter: waiter} ->
            waiter
        end
      end)

    assert {_from, _timeout_ref} = waiter

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

    assert {:response, :nofin, 200, []} = StreamResponseProcess.await(pid, 100)
    assert {:error, {:connection_error, :shutdown}} = StreamResponseProcess.await(pid, 100)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
    assert {:error, {:connection_error, :closed}} = StreamResponseProcess.await(pid, 100)
  end

  test "ignores connection errors after terminal trailers are already queued" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:gun_trailers, self(), make_ref(), [{"grpc-status", "0"}]})
    send(pid, {:connection_down, :shutdown})

    assert {:trailers, [{"grpc-status", "0"}]} = StreamResponseProcess.await(pid, 100)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end

  test "ignores duplicate terminal connection errors" do
    {:ok, pid} = StreamResponseProcess.start_link()
    monitor_ref = Process.monitor(pid)

    send(pid, {:connection_down, :first})
    send(pid, {:connection_down, :second})

    assert {:error, {:connection_error, :first}} = StreamResponseProcess.await(pid, 100)
    assert_receive {:DOWN, ^monitor_ref, :process, ^pid, :normal}, 500
  end
end

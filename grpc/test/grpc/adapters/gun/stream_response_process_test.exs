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
end

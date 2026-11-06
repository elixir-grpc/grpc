defmodule GRPC.Client.AdapterTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias GRPC.Client.Adapter
  alias GRPC.Client.Adapters.Gun.ConnectionProcess, as: GunConnectionProcess
  alias GRPC.Client.Adapters.Mint.ConnectionProcess, as: MintConnectionProcess

  @path "/routeguide.RouteGuide/GetFeature"
  @headers [{"authorization", "Bearer secret-token"}, {"te", "trailers"}]
  @body "secret-body"

  describe "connection process calls that exit" do
    test "Gun request: headers and body are redacted, the shape and path are kept" do
      pid = dead_pid()

      assert {:noproc, {GenServer, :call, [^pid, {:request, @path, :redacted, :redacted}, 5_000]}} =
               catch_exit(GunConnectionProcess.request(pid, @path, @headers, @body))
    end

    test "Gun open_stream and send_data are redacted" do
      pid = dead_pid()
      ref = make_ref()

      assert {:noproc, {GenServer, :call, [^pid, {:open_stream, @path, :redacted}, _]}} =
               catch_exit(GunConnectionProcess.open_stream(pid, @path, @headers))

      assert {:noproc, {GenServer, :call, [^pid, {:send_data, ^ref, :fin, :redacted}, _]}} =
               catch_exit(GunConnectionProcess.send_data(pid, ref, :fin, @body))
    end

    test "Mint request and stream_request_body are redacted" do
      pid = dead_pid()
      ref = make_ref()
      opts = [stream_response_pid: self()]

      assert {:noproc,
              {GenServer, :call,
               [^pid, {:request, "POST", @path, :redacted, :redacted, ^opts}, 5_000]}} =
               catch_exit(
                 MintConnectionProcess.request(pid, "POST", @path, @headers, @body, opts)
               )

      assert {:noproc, {GenServer, :call, [^pid, {:stream_body, ^ref, :redacted}, _]}} =
               catch_exit(MintConnectionProcess.stream_request_body(pid, ref, @body))
    end

    test "a call timeout to a busy process is redacted too" do
      pid = spawn_link(fn -> Process.sleep(:infinity) end)

      assert {:timeout, {GenServer, :call, [^pid, {:request, @path, :redacted, :redacted}, 50]}} =
               catch_exit(Adapter.call(pid, {:request, @path, @headers, @body}, 50))
    end

    test "the client span neither logs nor emits the headers or the body" do
      handler_id = {__MODULE__, make_ref()}

      :telemetry.attach(
        handler_id,
        [:grpc, :client, :rpc, :exception],
        &__MODULE__.forward_exception/4,
        self()
      )

      on_exit(fn -> :telemetry.detach(handler_id) end)
      pid = dead_pid()

      log =
        capture_log(fn ->
          catch_exit(
            GRPC.Telemetry.client_span(:stream, :request, fn ->
              GunConnectionProcess.request(pid, @path, @headers, @body)
            end)
          )
        end)

      assert log =~ @path
      refute log =~ "secret-token"
      refute log =~ "secret-body"

      assert_receive {:rpc_exception, %{kind: :exit, reason: reason}}
      refute inspect(reason) =~ "secret"
    end
  end

  describe "redact/1" do
    test "leaves other exit reasons and call messages untouched" do
      for reason <- [
            :normal,
            {:shutdown, :closed},
            {:timeout, {GenServer, :call, [self(), :disconnect, 5_000]}},
            {:noproc, {GenServer, :call, [self(), {:cancel, make_ref()}, 5_000]}}
          ] do
        assert Adapter.redact(reason) == reason
      end
    end
  end

  @doc false
  def forward_exception(_event, _measurements, metadata, test_pid) do
    send(test_pid, {:rpc_exception, metadata})
  end

  defp dead_pid do
    {pid, ref} = spawn_monitor(fn -> :ok end)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}
    pid
  end
end

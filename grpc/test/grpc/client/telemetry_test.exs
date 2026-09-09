defmodule GRPC.Client.TelemetryTest do
  use GRPC.Client.DataCase, async: true

  import ExUnit.CaptureLog

  alias GRPC.Client.Telemetry

  test "adds bounded local failure classification without changing the RPC result" do
    attach_telemetry([:grpc, :client, :rpc, :stop])
    error = GRPC.RPCError.exception(GRPC.Status.unavailable(), "connection unavailable")

    assert {:error, ^error} =
             Telemetry.client_span(%GRPC.Client.Stream{}, :request, fn ->
               Telemetry.mark_rpc_failure(:local_pre_send, :connection_unavailable)
               {:error, error}
             end)

    assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                    %{
                      result: {:error, ^error},
                      failure_stage: :local_pre_send,
                      failure_reason: :connection_unavailable
                    }}
  end

  test "adds failure classification to exception telemetry" do
    attach_telemetry([:grpc, :client, :rpc, :exception])
    error = GRPC.RPCError.exception(GRPC.Status.unavailable(), "connection unavailable")

    capture_log(fn ->
      assert_raise GRPC.RPCError, fn ->
        Telemetry.client_span(%GRPC.Client.Stream{}, :request, fn ->
          Telemetry.mark_rpc_failure(:local_pre_send, :connection_unavailable)
          raise error
        end)
      end
    end)

    assert_receive {:telemetry, [:grpc, :client, :rpc, :exception], _measurements,
                    %{
                      failure_stage: :local_pre_send,
                      failure_reason: :connection_unavailable
                    }}
  end

  test "does not classify an unmarked RPC error" do
    attach_telemetry([:grpc, :client, :rpc, :stop])
    error = GRPC.RPCError.exception(GRPC.Status.internal(), "remote error")

    assert {:error, ^error} =
             Telemetry.client_span(%GRPC.Client.Stream{}, :request, fn -> {:error, error} end)

    assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements, metadata}
    refute Map.has_key?(metadata, :failure_stage)
    refute Map.has_key?(metadata, :failure_reason)
  end
end

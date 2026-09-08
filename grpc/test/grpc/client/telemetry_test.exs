defmodule GRPC.Client.TelemetryTest do
  use GRPC.Client.DataCase, async: true

  alias GRPC.Client.Telemetry

  test "adds bounded local failure classification without changing the RPC result" do
    attach_telemetry([:grpc, :client, :rpc, :stop])
    error = GRPC.RPCError.exception(GRPC.Status.resource_exhausted(), "at capacity")

    assert {:error, ^error} =
             Telemetry.client_span(%GRPC.Client.Stream{}, :request, fn ->
               Telemetry.mark_rpc_failure(:local_pre_send, :capacity)
               {:error, error}
             end)

    assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                    %{
                      result: {:error, ^error},
                      failure_stage: :local_pre_send,
                      failure_reason: :capacity
                    }}
  end

  test "classifies an unmarked RPC error as a remote response" do
    attach_telemetry([:grpc, :client, :rpc, :stop])
    error = GRPC.RPCError.exception(GRPC.Status.internal(), "remote error")

    assert {:error, ^error} =
             Telemetry.client_span(%GRPC.Client.Stream{}, :request, fn -> {:error, error} end)

    assert_receive {:telemetry, [:grpc, :client, :rpc, :stop], _measurements,
                    %{failure_stage: :remote, failure_reason: nil}}
  end
end

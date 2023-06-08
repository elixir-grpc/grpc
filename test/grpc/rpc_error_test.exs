defmodule GRPC.RPCErrorTest do
  use GRPC.DataCase, async: true

  import GRPC.RPCError, only: [is_rpc_error: 2]

  alias GRPC.Status
  alias GRPC.RPCError

  defmodule HandlingWithFunctionGuard do
    @cancelled Status.cancelled()
    @unknown Status.unknown()

    def handle(err) when is_rpc_error(err, @cancelled) do
      :cancelled
    end

    def handle(err) when is_rpc_error(err, @unknown) do
      :unknown
    end

    def handle(_err) do
      :fallback
    end
  end

  describe "is_rpc_error/2 guard clause" do
    test "returns true for GRPC.RPCError structs with same status" do
      exception = GRPC.RPCError.new(:unknown)

      assert is_rpc_error(exception, Status.unknown())
    end

    test "returns false for GRPC.RPCError structs with different status" do
      exception = GRPC.RPCError.new(:unknown)

      refute is_rpc_error(exception, Status.cancelled())
    end

    test "returns false for others types of input" do
      exception = %{status: :unknown}

      refute is_rpc_error(exception, Status.unknown())
    end

    test "supports clause guarding in function heads" do
      assert HandlingWithFunctionGuard.handle(GRPC.RPCError.new(:unknown)) == :unknown
      assert HandlingWithFunctionGuard.handle(GRPC.RPCError.new(:cancelled)) == :cancelled
      assert HandlingWithFunctionGuard.handle(GRPC.RPCError.new(:not_found)) == :fallback
    end
  end

  describe "new/1" do
    test "creates the GRPC.RPCError struct with a default message" do
      assert %RPCError{status: code, message: message} = RPCError.new(:unknown)
      assert code == Status.unknown()
      refute is_nil(message)
    end

    test "creates the GRPC.RPCError struct with its own predefined message and status code" do
      # small sampling of statuses. ok must be here because its an edge case
      for status_name <- [:ok, :cancelled, :unknown] do
        status = apply(Status, status_name, [])

        assert %RPCError{status: status, message: Status.status_message(status)} ==
                 RPCError.new(status_name)
      end
    end
  end
end

defmodule GRPC.RPCErrorTest do
  use GRPC.DataCase, async: true

  import GRPC.RPCError, only: [is_rpc_error: 2]

  alias GRPC.Status
  alias GRPC.RPCError

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
  end

  describe "new/1" do
    test "creates the GRPC.RPCError struct with a default message" do
      assert %RPCError{status: code, message: message} = RPCError.new(:unknown)
      assert code == Status.unknown()
      refute is_nil(message)
    end

    test "creates the GRPC.RPCError struct with its own predefined message and status code" do
      for {name, expected_code, expected_message} <- grpc_status_codes_messages_factory(),
          not is_nil(expected_message) do
        assert %RPCError{status: code, message: message} = RPCError.new(name)
        assert expected_code == code
        assert expected_message == message
      end
    end
  end
end

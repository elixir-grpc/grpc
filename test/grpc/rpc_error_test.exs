defmodule GRPC.RPCErrorTest do
  use ExUnit.Case, async: true

  import GRPC.RPCError, only: [is_rpc_error: 2]

  alias GRPC.RPCError

  describe "is_rpc_error/2 guard clause" do
    test "returns true for GRPC.RPCError structs with same status" do
      exception = GRPC.RPCError.new(:unknown)

      assert is_rpc_error(exception, 2)
    end

    test "returns false for GRPC.RPCError structs with different status" do
      exception = GRPC.RPCError.new(:unknown)

      refute is_rpc_error(exception, 1)
    end

    test "returns false for others types of input" do
      exception = %{status: :unknown}

      refute is_rpc_error(exception, 2)
    end
  end

  describe "new/1" do
    test "creates GRPC.RPCError struct with a default message" do
      assert %RPCError{status: code, message: message} = RPCError.new(:unknown)
      assert code == 2
      refute is_nil(message)
    end
  end
end

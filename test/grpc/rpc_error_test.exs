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

  describe "exception/1" do
    test "creates RPC error with status atom" do
      error = GRPC.RPCError.exception(status: :invalid_argument)

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == GRPC.Status.status_message(GRPC.Status.invalid_argument())
      assert error.details == nil
    end

    test "creates RPC error with status integer" do
      error = GRPC.RPCError.exception(status: 3)

      assert error.status == 3
      assert error.message == GRPC.Status.status_message(3)
      assert error.details == nil
    end

    test "creates RPC error with custom message" do
      error = GRPC.RPCError.exception(status: :invalid_argument, message: "Custom error message")

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == "Custom error message"
      assert error.details == nil
    end

    test "creates RPC error with details" do
      details = [
        %Google.Protobuf.Any{
          type_url: "type.googleapis.com/google.rpc.ErrorInfo",
          value: Google.Rpc.ErrorInfo.encode(%Google.Rpc.ErrorInfo{reason: "TEST"})
        }
      ]

      error = GRPC.RPCError.exception(status: :invalid_argument, details: details)

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == GRPC.Status.status_message(GRPC.Status.invalid_argument())
      assert error.details == details
    end

    test "creates RPC error with all parameters" do
      details = [
        %Google.Protobuf.Any{
          type_url: "type.googleapis.com/google.rpc.ErrorInfo",
          value: Google.Rpc.ErrorInfo.encode(%Google.Rpc.ErrorInfo{reason: "TEST"})
        }
      ]

      error =
        GRPC.RPCError.exception(
          status: :invalid_argument,
          message: "Custom message",
          details: details
        )

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == "Custom message"
      assert error.details == details
    end
  end

  describe "exception/2" do
    test "creates RPC error with atom status and message" do
      error = GRPC.RPCError.exception(:invalid_argument, "Custom message")

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == "Custom message"
      assert error.details == nil
    end

    test "creates RPC error with integer status and message" do
      error = GRPC.RPCError.exception(3, "Custom message")

      assert error.status == 3
      assert error.message == "Custom message"
      assert error.details == nil
    end
  end

  describe "from_grpc_status_details_bin/1" do
    test "handles corrupted error details gracefully" do
      status = GRPC.Status.internal()
      message = "Test error"
      invalid_details = "!!!invalid!!!"

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: message,
          encoded_details_bin: invalid_details
        })

      assert error.status == 13
      assert error.message == "Test error"
      refute error.details
    end

    test "handles corrupted protobuf in details" do
      status = GRPC.Status.internal()
      message = "Test error"
      corrupted_protobuf = Base.encode64("not a valid protobuf message", padding: false)

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: message,
          encoded_details_bin: corrupted_protobuf
        })

      assert error.status == 13
      assert error.message == "Test error"
      refute error.details
    end

    test "handles nil encoded_details_bin" do
      status = GRPC.Status.internal()
      message = "Test error"

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: message,
          encoded_details_bin: nil
        })

      assert error.status == 13
      assert error.message == "Test error"
      refute error.details
    end

    test "handles nil message" do
      status = GRPC.Status.internal()

      encoded_details =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: "Status message",
          details: []
        })

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: nil,
          encoded_details_bin: encoded_details
        })

      assert error.status == 13
      assert error.message == "Status message"
      assert error.details == []
    end

    test "handles valid encoded details" do
      status = GRPC.Status.internal()
      message = "Test error"

      error_info = %Google.Rpc.ErrorInfo{
        reason: "TEST_REASON",
        domain: "example.com",
        metadata: %{"key" => "value"}
      }

      detail = %Google.Protobuf.Any{
        type_url: "type.googleapis.com/google.rpc.ErrorInfo",
        value: Google.Rpc.ErrorInfo.encode(error_info)
      }

      encoded_details =
        GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
          code: status,
          message: "Status message",
          details: [detail]
        })

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: message,
          encoded_details_bin: encoded_details
        })

      assert error.status == 13
      assert error.message == "Status message"
      assert error.details == [detail]
    end

    test "handles empty encoded details" do
      status = GRPC.Status.internal()
      message = "Test error"

      error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: message,
          encoded_details_bin: ""
        })

      assert error.status == 13
      assert error.message == ""
      assert error.details == []
    end
  end
end

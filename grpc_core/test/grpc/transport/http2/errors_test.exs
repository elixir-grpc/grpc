defmodule GRPC.Transport.HTTP2.ErrorsTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Errors

  describe "error codes" do
    test "returns correct code for no_error" do
      assert Errors.no_error() == 0x0
    end

    test "returns correct code for protocol_error" do
      assert Errors.protocol_error() == 0x1
    end

    test "returns correct code for internal_error" do
      assert Errors.internal_error() == 0x2
    end

    test "returns correct code for flow_control_error" do
      assert Errors.flow_control_error() == 0x3
    end

    test "returns correct code for settings_timeout" do
      assert Errors.settings_timeout() == 0x4
    end

    test "returns correct code for stream_closed" do
      assert Errors.stream_closed() == 0x5
    end

    test "returns correct code for frame_size_error" do
      assert Errors.frame_size_error() == 0x6
    end

    test "returns correct code for refused_stream" do
      assert Errors.refused_stream() == 0x7
    end

    test "returns correct code for cancel" do
      assert Errors.cancel() == 0x8
    end

    test "returns correct code for compression_error" do
      assert Errors.compression_error() == 0x9
    end

    test "returns correct code for connect_error" do
      assert Errors.connect_error() == 0xA
    end

    test "returns correct code for enhance_your_calm" do
      assert Errors.enhance_your_calm() == 0xB
    end

    test "returns correct code for inadequate_security" do
      assert Errors.inadequate_security() == 0xC
    end

    test "returns correct code for http_1_1_requires" do
      assert Errors.http_1_1_requires() == 0xD
    end
  end

  describe "ConnectionError exception" do
    test "can be raised with message" do
      assert_raise Errors.ConnectionError, "test error", fn ->
        raise Errors.ConnectionError, message: "test error"
      end
    end

    test "can be raised with error code" do
      exception = %Errors.ConnectionError{message: "error", error_code: 0x1}
      assert exception.error_code == 0x1
    end

    test "can be raised with both message and error code" do
      exception = %Errors.ConnectionError{
        message: "protocol violation",
        error_code: Errors.protocol_error()
      }

      assert exception.message == "protocol violation"
      assert exception.error_code == 0x1
    end
  end

  describe "StreamError exception" do
    test "can be raised with message" do
      assert_raise Errors.StreamError, "stream error", fn ->
        raise Errors.StreamError, message: "stream error"
      end
    end

    test "can be raised with stream_id" do
      exception = %Errors.StreamError{message: "error", stream_id: 1}
      assert exception.stream_id == 1
    end

    test "can be raised with all fields" do
      exception = %Errors.StreamError{
        message: "stream closed",
        error_code: Errors.stream_closed(),
        stream_id: 3
      }

      assert exception.message == "stream closed"
      assert exception.error_code == 0x5
      assert exception.stream_id == 3
    end
  end
end

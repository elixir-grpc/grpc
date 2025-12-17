defmodule GRPC.Transport.HTTP2.ErrorsTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Errors

  describe "error codes" do
    test "no_error returns 0x0" do
      assert Errors.no_error() == 0x0
    end

    test "protocol_error returns 0x1" do
      assert Errors.protocol_error() == 0x1
    end

    test "internal_error returns 0x2" do
      assert Errors.internal_error() == 0x2
    end

    test "flow_control_error returns 0x3" do
      assert Errors.flow_control_error() == 0x3
    end

    test "settings_timeout returns 0x4" do
      assert Errors.settings_timeout() == 0x4
    end

    test "stream_closed returns 0x5" do
      assert Errors.stream_closed() == 0x5
    end

    test "frame_size_error returns 0x6" do
      assert Errors.frame_size_error() == 0x6
    end

    test "refused_stream returns 0x7" do
      assert Errors.refused_stream() == 0x7
    end

    test "cancel returns 0x8" do
      assert Errors.cancel() == 0x8
    end

    test "compression_error returns 0x9" do
      assert Errors.compression_error() == 0x9
    end

    test "connect_error returns 0xA" do
      assert Errors.connect_error() == 0xA
    end

    test "enhance_your_calm returns 0xB" do
      assert Errors.enhance_your_calm() == 0xB
    end

    test "inadequate_security returns 0xC" do
      assert Errors.inadequate_security() == 0xC
    end

    test "http_1_1_requires returns 0xD" do
      assert Errors.http_1_1_requires() == 0xD
    end
  end

  describe "ConnectionError" do
    test "can be raised with message and error code" do
      assert_raise Errors.ConnectionError, "test message", fn ->
        raise Errors.ConnectionError, message: "test message", error_code: 0x1
      end
    end
  end

  describe "StreamError" do
    test "can be raised with message, error code and stream_id" do
      assert_raise Errors.StreamError, "test message", fn ->
        raise Errors.StreamError, message: "test message", error_code: 0x1, stream_id: 1
      end
    end
  end
end

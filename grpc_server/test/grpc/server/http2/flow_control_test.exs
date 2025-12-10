defmodule GRPC.Transport.HTTP2.FlowControlTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.FlowControl

  import Bitwise

  @max_window_size (1 <<< 31) - 1
  @min_window_size 1 <<< 30

  describe "compute_recv_window/2" do
    test "returns no increment when window is still large" do
      # Window is still above minimum, no need to update yet
      recv_window_size = @min_window_size + 1000
      data_size = 500

      assert {new_window, 0} = FlowControl.compute_recv_window(recv_window_size, data_size)
      assert new_window == recv_window_size - data_size
    end

    test "returns increment when window falls below minimum" do
      # Window falls below minimum, need to send WINDOW_UPDATE
      recv_window_size = @min_window_size + 100
      data_size = 200

      assert {new_window, increment} =
               FlowControl.compute_recv_window(recv_window_size, data_size)

      assert new_window > recv_window_size - data_size
      assert increment > 0
    end

    test "respects maximum window size" do
      # Even when requesting update, should not exceed max window size
      recv_window_size = @min_window_size - 1000
      data_size = 100

      assert {new_window, increment} =
               FlowControl.compute_recv_window(recv_window_size, data_size)

      assert new_window <= @max_window_size
      assert increment > 0
    end

    test "handles small recv_window_size" do
      recv_window_size = 1000
      data_size = 500

      assert {new_window, increment} =
               FlowControl.compute_recv_window(recv_window_size, data_size)

      assert new_window > recv_window_size - data_size
      assert increment > 0
    end

    test "handles edge case when recv_window equals min_window_size" do
      recv_window_size = @min_window_size
      data_size = 1

      assert {new_window, increment} =
               FlowControl.compute_recv_window(recv_window_size, data_size)

      assert new_window > recv_window_size - data_size
      assert increment > 0
    end

    test "handles large data size" do
      recv_window_size = @max_window_size
      data_size = @min_window_size + 1000

      assert {new_window, increment} =
               FlowControl.compute_recv_window(recv_window_size, data_size)

      # Window should still be positive after large data consumption
      assert new_window >= 0
      # May or may not need increment depending on resulting window size
      assert is_integer(increment)
    end
  end
end

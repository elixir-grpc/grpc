defmodule GRPC.Transport.HTTP2.FlowControlTest do
  use ExUnit.Case, async: true

  import Bitwise
  alias GRPC.Transport.HTTP2.FlowControl

  describe "compute_recv_window/2" do
    test "returns correct window size when above minimum threshold" do
      # Start with a large window above the minimum threshold (1GB)
      large_window = (1 <<< 30) + 1_000_000
      # Receive small amount of data
      {new_window, increment} = FlowControl.compute_recv_window(large_window, 1_024)

      # Should still be above threshold, no increment needed
      assert new_window == large_window - 1_024
      assert increment == 0
    end

    test "returns window increment when below minimum threshold" do
      # Start with minimum threshold + 1 (1GB + 1)
      min_threshold = 1 <<< 30
      # Receive enough data to go below threshold
      data_size = 2

      {new_window, increment} = FlowControl.compute_recv_window(min_threshold + 1, data_size)

      # Should have sent a WINDOW_UPDATE
      assert increment > 0
      # New window should be original - data + increment
      assert new_window == min_threshold + 1 - data_size + increment
    end

    test "respects maximum window increment" do
      # Maximum increment is 2^31 - 1
      max_increment = (1 <<< 31) - 1

      # Start with a very small window
      {new_window, increment} = FlowControl.compute_recv_window(1, 0)

      # Increment should not exceed maximum
      assert increment <= max_increment
      assert new_window == 1 + increment
    end

    test "respects maximum window size" do
      # Maximum window size is 2^31 - 1
      max_window = (1 <<< 31) - 1

      # Start with a small window that would overflow
      {new_window, _increment} = FlowControl.compute_recv_window(1, 0)

      # New window should not exceed maximum
      assert new_window <= max_window
    end

    test "handles zero data size" do
      initial_window = 65_535
      {new_window, increment} = FlowControl.compute_recv_window(initial_window, 0)

      # Window might change if below threshold (function computes new window)
      # Just verify it's reasonable
      assert new_window >= initial_window
      assert increment >= 0
    end

    test "handles large data sizes" do
      initial_window = 1 <<< 30
      # Receive 100MB of data
      data_size = 100 * 1024 * 1024

      {new_window, _increment} = FlowControl.compute_recv_window(initial_window, data_size)

      # Window should be decreased by data size, then potentially increased by increment
      # Just verify the window is reasonable (non-negative and less than max)
      assert new_window >= 0
      assert new_window <= (1 <<< 31) - 1
    end
  end

  describe "update_window/2" do
    test "updates window size with positive increment" do
      assert {:ok, 100} = FlowControl.update_window(50, 50)
    end

    test "updates window size with negative increment" do
      assert {:ok, 50} = FlowControl.update_window(100, -50)
    end

    test "allows window size of zero" do
      assert {:ok, 0} = FlowControl.update_window(50, -50)
    end

    test "returns error when window size would exceed maximum" do
      max_window = (1 <<< 31) - 1

      assert {:error, :flow_control_error} =
               FlowControl.update_window(max_window, 1)
    end

    test "returns error when increment causes overflow" do
      # Window size is at max, any positive increment should fail
      max_window = (1 <<< 31) - 1

      assert {:error, :flow_control_error} =
               FlowControl.update_window(max_window, 100)
    end

    test "allows update to exactly maximum window size" do
      max_window = (1 <<< 31) - 1

      assert {:ok, ^max_window} = FlowControl.update_window(max_window - 100, 100)
    end

    test "handles large positive increments" do
      large_increment = 1_000_000

      assert {:ok, 1_000_050} = FlowControl.update_window(50, large_increment)
    end

    test "handles large negative increments" do
      large_decrement = -1_000_000

      assert {:ok, 0} = FlowControl.update_window(1_000_000, large_decrement)
    end

    test "RFC 9113 compliance - maximum window size is 2^31-1" do
      # RFC 9113 §6.9.1
      max_window = (1 <<< 31) - 1

      assert {:ok, ^max_window} = FlowControl.update_window(0, max_window)
      assert {:error, :flow_control_error} = FlowControl.update_window(max_window, 1)
    end
  end
end

defmodule GRPC.HTTP2.FlowControl do
  @moduledoc false
  # Helpers for working with flow control window calculations

  import Bitwise

  @max_window_increment (1 <<< 31) - 1
  @max_window_size (1 <<< 31) - 1
  @min_window_size 1 <<< 30

  @spec compute_recv_window(non_neg_integer(), non_neg_integer()) ::
          {non_neg_integer(), non_neg_integer()}
  def compute_recv_window(recv_window_size, data_size) do
    # This is what our window size will be after receiving data_size bytes
    recv_window_size = recv_window_size - data_size

    if recv_window_size > @min_window_size do
      # We have room to go before we need to update our window
      {recv_window_size, 0}
    else
      # We want our new window to be as large as possible, but are limited by both the maximum size
      # of a WINDOW_UPDATE frame (max_window_increment) and the maximum window size (max_window_size)
      window_increment =
        min(@max_window_increment, @max_window_size - recv_window_size)

      {recv_window_size + window_increment, window_increment}
    end
  end

  @doc """
  Updates window size by increment, ensuring it doesn't exceed maximum.
  """
  @spec update_window(non_neg_integer(), integer()) ::
          {:ok, non_neg_integer()} | {:error, :flow_control_error}
  def update_window(current_size, increment) do
    new_size = current_size + increment

    if new_size > @max_window_size do
      {:error, :flow_control_error}
    else
      {:ok, new_size}
    end
  end
end

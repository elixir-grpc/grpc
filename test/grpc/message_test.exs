defmodule GRPC.MessageTest do
  use ExUnit.Case, async: true

  doctest GRPC.Message

  test "compressor works" do
    message = String.duplicate("foo", 100)

    # 10th byte is the operating system ID
    assert {:ok,
            data =
              <<1, 0, 0, 0, 27, 31, 139, 8, 0, 0, 0, 0, 0, 0, _, 75, 203, 207, 79, 27, 69, 196,
                33, 0, 41, 249, 122, 62, 44, 1, 0, 0>>,
            32} = GRPC.Message.to_data(message, %{compressor: GRPC.Compressor.Gzip})

    assert {:ok, message} == GRPC.Message.from_data(%{compressor: GRPC.Compressor.Gzip}, data)
  end
end

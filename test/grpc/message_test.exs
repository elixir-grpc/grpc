defmodule GRPC.MessageTest do
  use ExUnit.Case, async: true

  doctest GRPC.Message

  import GRPC.Message

  test "complete? returns true" do
    assert true == complete?(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7, 8>>)
  end

  test "complete? returns false for uncompleted data" do
    assert false == complete?(<<0, 0, 0, 0, 8, 1, 2, 3, 4, 5, 6, 7>>)
  end

  test "complete? returns false for unmatched message" do
    assert false == complete?(<<0, 0, 0, 0>>)
  end
end

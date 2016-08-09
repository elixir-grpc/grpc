defmodule GRPC.Core.CompletionQueueTest do
  use ExUnit.Case, async: true

  test "create/0 works" do
    assert GRPC.Core.CompletionQueue.create
  end
end

defmodule GRPC.Core.CallTest do
  use ExUnit.Case, async: true

  test "create/7 works" do
    channel = GRPC.Core.Channel.create("localhost:50051", %{}, :this_channel_is_insecure)
    cq = GRPC.Core.CompletionQueue.create
    deadline = :os.system_time(:seconds) + 300
    call = GRPC.Core.Call.create(channel, nil, nil, cq, String.to_charlist("/foo/bar"), nil, deadline)
    assert call
  end
end

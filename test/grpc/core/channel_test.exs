defmodule GRPC.Core.ChannelTest do
  use ExUnit.Case, async: true

  test "create/3 works" do
    assert GRPC.Core.Channel.create("localhost:50051", %{}, :this_channel_is_insecure)
  end

  test "create/3 raises error when cred is wrong atom" do
    assert_raise ErlangError, ~r/must be atom :this_channel_is_insecure/, fn ->
      GRPC.Core.Channel.create("localhost:50051", %{}, :this_channel_is_insecur)
    end
  end

  test "create/3 raises error when channel is nil" do
    assert_raise ErlangError, ~r/Could not create an rpc channel/,  fn ->
      GRPC.Core.Channel.create("localhost:50051", %{}, %{})
    end
  end
end

defmodule GRPC.MessageLimitTest do
  # Sets the :max_message_length application env, which every other test reads.
  use ExUnit.Case, async: false

  alias GRPC.RPCError

  @limit 1024

  setup do
    Application.put_env(:grpc, :max_message_length, @limit)
    on_exit(fn -> Application.delete_env(:grpc, :max_message_length) end)
  end

  defp frame(size, flag \\ 0), do: <<flag, size::32, :binary.copy("a", size)::binary>>

  test "max_message_length/0 defaults to 2 GiB and reads the application env" do
    assert GRPC.Message.max_message_length() == @limit

    Application.delete_env(:grpc, :max_message_length)
    assert GRPC.Message.max_message_length() == Bitwise.bsl(1, 31)
  end

  describe "to_data/2" do
    test "rejects a message over the application env limit" do
      assert {:ok, _, _} = GRPC.Message.to_data(:binary.copy("a", @limit))

      assert {:error, "Encoded message is too large (1025 bytes)"} =
               GRPC.Message.to_data(:binary.copy("a", @limit + 1))
    end

    test "the :max_message_length option wins over the application env" do
      assert {:ok, _, _} =
               GRPC.Message.to_data(:binary.copy("a", @limit + 1), max_message_length: 2048)

      assert {:error, _} = GRPC.Message.to_data("abc", max_message_length: 2)
    end
  end

  describe "from_data/2" do
    test "returns RESOURCE_EXHAUSTED for a prefix over the limit, without needing the body" do
      for compressor <- [nil, GRPC.Compressor.Gzip] do
        assert {:error, %RPCError{status: 8, message: message}} =
                 GRPC.Message.from_data(%{compressor: compressor}, <<0, @limit + 1::32, "short">>)

        assert message == "Received message is too large (1025 bytes, the limit is 1024 bytes)"
      end
    end

    test "accepts a message at the limit" do
      assert {:ok, message, <<>>} = GRPC.Message.from_data(%{compressor: nil}, frame(@limit))
      assert byte_size(message) == @limit
    end

    test "applies to a compressed message by its length on the wire" do
      body = frame(@limit + 1, 1)

      assert {:error, %RPCError{status: 8}} =
               GRPC.Message.from_data(%{compressor: GRPC.Compressor.Gzip}, body)
    end

    test "leaves a short, malformed buffer to the INTERNAL error" do
      assert {:error, %RPCError{status: 13}} =
               GRPC.Message.from_data(%{compressor: nil}, <<0, 0>>)
    end
  end

  describe "from_data/1" do
    test "raises RESOURCE_EXHAUSTED for a prefix over the limit" do
      assert_raise RPCError, ~r/too large \(1025 bytes/, fn ->
        GRPC.Message.from_data(<<0, @limit + 1::32, "short">>)
      end
    end
  end

  describe "get_message/1,2" do
    test "raise RESOURCE_EXHAUSTED once the prefix is over the limit, before the body arrives" do
      partial = <<0, @limit + 1::32, "only a part of the body">>

      for fun <- [
            fn -> GRPC.Message.get_message(partial) end,
            fn -> GRPC.Message.get_message(partial, nil) end,
            fn -> GRPC.Message.get_message(partial, GRPC.Compressor.Gzip) end
          ] do
        error = assert_raise RPCError, fun
        assert error.status == 8

        assert error.message ==
                 "Received message is too large (1025 bytes, the limit is 1024 bytes)"
      end
    end

    test "return messages up to the limit, and wait for more data on a partial one" do
      body = frame(@limit)

      assert {{0, message}, <<>>} = GRPC.Message.get_message(body)
      assert byte_size(message) == @limit
      assert {{0, ^message}, <<>>} = GRPC.Message.get_message(body, nil)

      partial = binary_part(body, 0, 100)
      assert GRPC.Message.get_message(partial) == false
      assert GRPC.Message.get_message(partial, nil) == partial
      assert GRPC.Message.get_message(<<0, 0>>) == false
    end
  end
end

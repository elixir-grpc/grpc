defmodule GRPC.Transport.HTTP2TimeoutTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2

  # A bare map suffices: GRPC.Client.Stream lives in the grpc package, unreachable from here.
  @stream %{
    codec: GRPC.Codec.Proto,
    compressor: nil,
    accepted_compressors: [],
    channel: %{headers: %{}},
    headers: %{}
  }

  defp timeout_header(opts) do
    HTTP2.client_headers_without_reserved(@stream, opts)
    |> Enum.find(fn {k, _v} -> k == "grpc-timeout" end)
  end

  describe "grpc-timeout header" do
    test "an integer timeout is sent in milliseconds" do
      assert timeout_header(%{timeout: 5}) == {"grpc-timeout", "5m"}
      assert timeout_header(%{timeout: 1500}) == {"grpc-timeout", "1500m"}
    end

    test ":infinity and nil send no deadline" do
      assert timeout_header(%{timeout: :infinity}) == nil
      assert timeout_header(%{timeout: nil}) == nil
      assert timeout_header(%{}) == nil
    end
  end
end

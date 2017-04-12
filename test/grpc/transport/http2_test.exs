defmodule GRPC.Transport.HTTP2Test do
  use ExUnit.Case, async: true
  alias GRPC.Channel
  alias GRPC.Transport.HTTP2

  @channel %Channel{scheme: "http", host: "grpc.io"}

  test "client_headers/3 returns basic headers" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, grpc_version: "1.0.0")
    assert headers == [
      {":method", "POST"},
      {":scheme", "http"},
      {":path", "/foo/bar"},
      {":authority", "grpc.io"},
      {"content-type", "application/grpc+proto"},
      {"user-agent", "grpc-elixir/1.0.0"},
      {"te", "trailers"}
    ]
  end

  test "client_headers/3 returns grpc-encoding" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, send_encoding: "gzip")
    assert List.last(headers) == {"grpc-encoding", "gzip"}
  end

  test "client_headers/3 returns custom metadata" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, metadata: %{foo: "bar", foo1: :bar1})
    assert [{"foo1", "bar1"}, {"foo", "bar"} | _] = Enum.reverse(headers)
  end

  test "client_headers/3 returns custom metadata with *-bin key" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, metadata: %{"key1-bin" => "abc", "key2-bin" => <<194, 128>>})
    assert [{"key2-bin", "woA="}, {"key1-bin", "YWJj"} | _] = Enum.reverse(headers)
  end

  test "client_headers/3 rejects reserved headers in metadata" do
    stream = %{channel: @channel, path: "/foo/bar"}
    metadata = %{"foo" => "bar", ":foo" => ":bar", "grpc-foo" => "bar", "content-type" => "bar", "te" => "et"}
    headers = HTTP2.client_headers(stream, metadata: metadata)
    assert [{"foo", "bar"}, {"te", "trailers"} | _] = Enum.reverse(headers)
  end

  test "client_headers/3 downcase keys of metadata" do
    stream = %{channel: @channel, path: "/foo/bar"}
    metadata = %{:Foo => "bar", "Foo-Bar" => "bar"}
    headers = HTTP2.client_headers(stream, metadata: metadata)
    assert [{"foo-bar", "bar"}, {"foo", "bar"} | _] = Enum.reverse(headers)
  end

  test "client_headers/3 has timeout with :deadline option" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, deadline: DateTime.utc_now)
    assert [{"grpc-timeout", "0u"} | _] = Enum.reverse(headers)
  end

  test "client_headers/3 has timeout with :timeout option" do
    stream = %{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, timeout: 5)
    assert [{"grpc-timeout", "5u"} | _] = Enum.reverse(headers)
  end
end

defmodule GRPC.CallTest do
  use ExUnit.Case, async: true
  alias GRPC.{Channel, Call}

  test "compose_headers/3 returns basic headers" do
    channel = %Channel{scheme: "http", host: "grpc.io"}
    headers = Call.compose_headers(channel, "/foo/bar", grpc_version: "1.0.0")
    assert headers == [
      {":method", "POST"},
      {":scheme", "http"},
      {":path", "/foo/bar"},
      {":authority", "grpc.io"},
      {"content-type", "application/grpc"},
      {"user-agent", "grpc-elixir/1.0.0"},
      {"te", "trailers"}
    ]
  end

  test "compose_headers/3 returns grpc-encoding" do
    channel = %Channel{}
    headers = Call.compose_headers(channel, "/foo/bar", send_encoding: "gzip")
    assert List.last(headers) == {"grpc-encoding", "gzip"}
  end

  test "compose_headers/3 returns custom metadata" do
    channel = %Channel{}
    headers = Call.compose_headers(channel, "/foo/bar", metadata: %{foo: "bar", foo1: :bar1})
    assert [{"foo1", "bar1"}, {"foo", "bar"} | _] = Enum.reverse(headers)
  end

  test "compose_headers/3 returns custom metadata with *-bin key" do
    channel = %Channel{}
    headers = Call.compose_headers(channel, "/foo/bar", metadata: %{"key1-bin" => "abc", "key2-bin" => <<194, 128>>})
    assert [{"key2-bin", "woA="}, {"key1-bin", "YWJj"} | _] = Enum.reverse(headers)
  end

  test "compose_headers/3 rejects reserved headers in metadata" do
    channel = %Channel{}
    metadata = %{"foo" => "bar", ":foo" => ":bar", "grpc-foo" => "bar", "content-type" => "bar", "te" => "et"}
    headers = Call.compose_headers(channel, "/foo/bar", metadata: metadata)
    assert [{"foo", "bar"}, {"te", "trailers"} | _] = Enum.reverse(headers)
  end

  test "compose_headers/3 downcase keys of metadata" do
    channel = %Channel{}
    metadata = %{:Foo => "bar", "Foo-Bar" => "bar"}
    headers = Call.compose_headers(channel, "/foo/bar", metadata: metadata)
    assert [{"foo-bar", "bar"}, {"foo", "bar"} | _] = Enum.reverse(headers)
  end

  test "compose_headers/3 has timeout with :deadline option" do
    channel = %Channel{}
    headers = Call.compose_headers(channel, "/foo/bar", deadline: DateTime.utc_now)
    assert [{"grpc-timeout", "0u"} | _] = Enum.reverse(headers)
  end

  test "compose_headers/3 has timeout with :timeout option" do
    channel = %Channel{}
    headers = Call.compose_headers(channel, "/foo/bar", timeout: 5)
    assert [{"grpc-timeout", "5u"} | _] = Enum.reverse(headers)
  end
end

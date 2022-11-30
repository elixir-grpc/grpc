defmodule GRPC.Transport.HTTP2Test do
  use ExUnit.Case, async: true
  alias GRPC.{Channel, Status}
  alias GRPC.Transport.HTTP2

  alias GRPC.Client.Stream
  alias GRPC.Server.Stream, as: ServerStream

  @channel %Channel{scheme: "http", host: "grpc.io"}

  defp assert_header({key, _v} = pair, headers) do
    assert pair == Enum.find(headers, nil, fn {k, _v} -> if k == key, do: true end)
  end

  test "client_headers/3 returns basic headers" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{grpc_version: "1.0.0"})

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

  test "client_headers/3 returns grpc-encoding" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{grpc_encoding: "gzip"})
    assert_header({"grpc-encoding", "gzip"}, headers)
  end

  test "client_headers/3 returns custom metadata" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{metadata: %{foo: "bar", foo1: :bar1}})
    assert_header({"foo1", "bar1"}, headers)
    assert_header({"foo", "bar"}, headers)
  end

  test "client_headers/3 returns custom metadata with *-bin key" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}

    headers =
      HTTP2.client_headers(stream, %{metadata: %{"key1-bin" => "abc", "key2-bin" => <<194, 128>>}})

    assert_header({"key1-bin", "YWJj"}, headers)
    assert_header({"key2-bin", "woA="}, headers)
  end

  test "client_headers/3 rejects reserved headers in metadata" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}

    metadata = %{
      "foo" => "bar",
      ":foo" => ":bar",
      "grpc-foo" => "bar",
      "content-type" => "bar",
      "te" => "et"
    }

    headers = HTTP2.client_headers(stream, %{metadata: metadata})
    assert_header({"te", "trailers"}, headers)
    assert_header({"foo", "bar"}, headers)
  end

  test "client_headers/3 downcase keys of metadata" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    metadata = %{:Foo => "bar", "Foo-Bar" => "bar"}
    headers = HTTP2.client_headers(stream, %{metadata: metadata})
    assert_header({"foo-bar", "bar"}, headers)
    assert_header({"foo", "bar"}, headers)
  end

  test "client_headers/3 merges metadata with same keys" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{metadata: [foo: "bar", foo: :bar1]})
    assert_header({"foo", "bar,bar1"}, headers)
  end

  test "client_headers/3 has timeout with :timeout option" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{timeout: 5})
    assert_header({"grpc-timeout", "5m"}, headers)
  end

  test "client_headers/3 support custom content-type" do
    stream = %Stream{channel: @channel, path: "/foo/bar"}
    headers = HTTP2.client_headers(stream, %{content_type: "application/grpc-custom"})

    assert {_, "application/grpc-custom"} =
             Enum.find(headers, fn {key, _} -> key == "content-type" end)
  end

  test "client_headers/3 support custom codec" do
    stream = %Stream{channel: @channel, path: "/foo/bar", codec: %{name: "custom-codec"}}
    headers = HTTP2.client_headers(stream, %{})

    assert {_, "application/grpc+custom-codec"} =
             Enum.find(headers, fn {key, _} -> key == "content-type" end)
  end

  test "server_headers/3 sets content-type based on the codec name" do
    for {expected_content_type, codec} <- [
          {"grpc-web-text", GRPC.Codec.WebText},
          {"grpc+erlpack", GRPC.Codec.Erlpack}
        ] do
      stream = %ServerStream{codec: codec}

      assert %{"content-type" => "application/" <> ^expected_content_type} =
               HTTP2.server_headers(stream)
    end
  end

  test "decode_headers/2 url decodes grpc-message" do
    trailers = HTTP2.server_trailers(Status.unknown(), "Unknown error")
    assert %{"grpc-message" => "Unknown error"} = HTTP2.decode_headers(trailers)
  end

  test "server_trailers/3 sets url encoded grpc-message" do
    assert %{"grpc-message" => "Ok"} = HTTP2.server_trailers(Status.ok(), "Ok")

    assert %{"grpc-message" => "Unknown%20error"} =
             HTTP2.server_trailers(Status.unknown(), "Unknown error")
  end
end

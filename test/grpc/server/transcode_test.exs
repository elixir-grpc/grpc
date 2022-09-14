defmodule GRPC.TranscodeTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Transcode

  test "map_requests/3 can map request body to protobuf struct" do
    body_request = %{"latitude" => 1, "longitude" => 2}
    {:ok, request} = Transcode.map_request(body_request, %{}, "", Routeguide.Point)
    assert Routeguide.Point.new(latitude: 1, longitude: 2) == request
  end

  test "map_requests/3 can merge request body with path bindings to protobuf struct" do
    body_request = %{"latitude" => 1}
    bindings = %{"longitude" => 2}
    {:ok, request} = Transcode.map_request(body_request, bindings, "", Routeguide.Point)
    assert Routeguide.Point.new(latitude: 1, longitude: 2) == request
  end

  test "build_route/1 returns a route with {http_method, route} based on the http rule" do
    rule = build_simple_rule(:get, "/v1/messages/{message_id}")
    assert {:get, {params, segments}} = Transcode.build_route(rule)
    assert [message_id: []] == params
    assert ["v1", "messages", {:message_id, []}] = segments
  end

  test "to_path/1 returns path segments as a string match" do
    rule = build_simple_rule(:get, "/v1/messages/{message_id}")
    assert spec = Transcode.build_route(rule)
    assert "/v1/messages/:message_id" = Transcode.to_path(spec)
  end

  test "to_path/1 returns path segments as a string when there's multiple bindings" do
    rule = build_simple_rule(:get, "/v1/users/{user_id}/messages/{message_id}")
    assert spec = Transcode.build_route(rule)
    assert "/v1/users/:user_id/messages/:message_id" = Transcode.to_path(spec)
  end

  describe "tokenize/2" do
    test "can tokenize simple paths" do
      assert [{:/, []}] = Transcode.tokenize("/")

      assert [{:/, []}, {:identifier, "v1", []}, {:/, []}, {:identifier, "messages", []}] =
               Transcode.tokenize("/v1/messages")
    end

    test "can tokenize simple paths with wildcards" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:identifier, "messages", []},
               {:/, []},
               {:*, []}
             ] == Transcode.tokenize("/v1/messages/*")
    end

    test "can tokenize simple variables" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:identifier, "messages", []},
               {:/, []},
               {:"{", []},
               {:identifier, "message_id", []},
               {:"}", []}
             ] == Transcode.tokenize("/v1/messages/{message_id}")
    end

    test "can tokenize variable assignments in bindings" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:"{", []},
               {:identifier, "name", []},
               {:=, []},
               {:identifier, "messages", []},
               {:"}", []}
             ] == Transcode.tokenize("/v1/{name=messages}")
    end

    test "can tokenize field paths in bindings" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:identifier, "messages", []},
               {:/, []},
               {:"{", []},
               {:identifier, "message_id", []},
               {:"}", []},
               {:/, []},
               {:"{", []},
               {:identifier, "sub.subfield", []},
               {:"}", []}
             ] == Transcode.tokenize("/v1/messages/{message_id}/{sub.subfield}")
    end
  end

  describe "parse/3" do
    test "can parse simple paths" do
      assert {[], []} ==
               "/"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse paths with identifiers" do
      assert {[], ["v1", "messages"]} ==
               "/v1/messages"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse paths with wildcards" do
      assert {[], ["v1", "messages", {:_, []}]} ==
               "/v1/messages/*"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse simple bindings with variables" do
      assert {[{:message_id, []}], ["v1", "messages", {:message_id, []}]} ==
               "/v1/messages/{message_id}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse bindings with variable assignment" do
      assert {[{:name, []}], ["v1", {:name, ["messages"]}]} ==
               "/v1/{name=messages}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse multiple bindings with variable assignment" do
      assert {[{:name, []}, {:message_id, []}], ["v1", {:name, ["messages"]}, {:message_id, []}]} ==
               "/v1/{name=messages}/{message_id}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse bindings with field paths " do
      assert {[sub: ["subfield"]], ["v1", "messages", {:sub, []}]} ==
               "/v1/messages/{sub.subfield}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "supports deeper nested field path " do
      assert {[sub: ["nested", "nested", "nested"]], ["v1", "messages", {:sub, []}]} ==
               "/v1/messages/{sub.nested.nested.nested}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end

    test "can parse multiple-bindings with field paths " do
      assert {[first: ["subfield"], second: ["subfield"]],
              ["v1", "messages", {:first, []}, {:second, []}]} ==
               "/v1/messages/{first.subfield}/{second.subfield}"
               |> Transcode.tokenize()
               |> Transcode.parse([], [])
    end
  end

  defp build_simple_rule(method, pattern) do
    Google.Api.HttpRule.new(pattern: {method, pattern})
  end
end

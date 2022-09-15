defmodule GRPC.TranscodeTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Transcode

  test "map_request/5 with HttpRule.body: '*'" do
    rule = Google.Api.HttpRule.new(body: "*")
    request_body = %{"latitude" => 1, "longitude" => 2}
    bindings = %{}
    qs = "latitude=10&longitude=20"

    assert {:ok, %Routeguide.Point{latitude: 1, longitude: 2}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/5 with empty HttpRule.body" do
    rule = Google.Api.HttpRule.new(body: "")
    request_body = %{"latitude" => 10, "longitude" => 20}
    bindings = %{"latitude" => 5}
    qs = "longitude=6"

    assert {:ok, %Routeguide.Point{latitude: 5, longitude: 6}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/2 with HttpRule.body: <field>" do
    rule = Google.Api.HttpRule.new(body: "location")
    request_body = %{"latitude" => 1, "longitude" => 2}
    bindings = %{"name" => "test"}

    assert {:ok, %Routeguide.Feature{name: "test", location: point}} =
             Transcode.map_request(rule, request_body, bindings, "name=Foo", Routeguide.Feature)

    assert point.latitude == 1
    assert point.longitude == 2
  end

  test "map_response_body/2 with empty HttpRule.response_body" do
    rule = Google.Api.HttpRule.new(response_body: "")
    request_body = %{"a" => "b"}

    assert request_body == Transcode.map_response_body(rule, request_body)
  end

  test "map_response_body/2 with HttpRule.response_body: <field>" do
    rule = Google.Api.HttpRule.new(response_body: "message")
    request_body = %{"message" => %{"a" => "b"}}

    assert %{"a" => "b"} == Transcode.map_response_body(rule, request_body)
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

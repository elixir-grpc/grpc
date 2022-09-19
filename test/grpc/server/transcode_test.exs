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
    request_body = %{message: %{a: "b"}}

    assert request_body == Transcode.map_response_body(rule, request_body)
  end

  test "map_response_body/2 with HttpRule.response_body: <field>" do
    rule = Google.Api.HttpRule.new(response_body: "message")
    request_body = %{message: %{a: "b"}}

    assert %{a: "b"} == Transcode.map_response_body(rule, request_body)
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

  test "map_route_bindings/2 should stringify the keys" do
    path_binding_atom = %{foo: "bar"}
    path_binding_string = %{foo: "bar"}

    assert %{"foo" => "bar"} == Transcode.map_path_bindings(path_binding_atom)
    assert %{"foo" => "bar"} == Transcode.map_path_bindings(path_binding_string)
  end

  test "map_route_bindings/2 with '.' delimited identifiers should create a nested map" do
    path_binding = %{"foo.bar.baz" => "biz"}
    assert %{"foo" => %{"bar" => %{"baz" => "biz"}}} == Transcode.map_path_bindings(path_binding)
  end

  defp build_simple_rule(method, pattern) do
    Google.Api.HttpRule.new(pattern: {method, pattern})
  end
end

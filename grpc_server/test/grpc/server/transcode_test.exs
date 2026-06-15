defmodule GRPC.TranscodeTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Transcode

  test "map_request/5 with HttpRule.body: '*'" do
    rule = %Google.Api.HttpRule{body: "*"}
    request_body = %{"latitude" => 1, "longitude" => 2}
    bindings = %{}
    qs = "latitude=10&longitude=20"

    assert {:ok, %Routeguide.Point{latitude: 1, longitude: 2}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/5 with empty HttpRule.body" do
    rule = %Google.Api.HttpRule{body: ""}
    request_body = %{"latitude" => 10, "longitude" => 20}
    bindings = %{"latitude" => 5}
    qs = "longitude=6"

    assert {:ok, %Routeguide.Point{latitude: 5, longitude: 6}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/2 with HttpRule.body: <field>" do
    rule = %Google.Api.HttpRule{body: "location"}
    request_body = %{"latitude" => 1, "longitude" => 2}
    bindings = %{"name" => "test"}

    assert {:ok, %Routeguide.Feature{name: "test", location: point}} =
             Transcode.map_request(rule, request_body, bindings, "", Routeguide.Feature)

    assert point.latitude == 1
    assert point.longitude == 2
  end

  test "map_response_body/2 with empty HttpRule.response_body" do
    rule = %Google.Api.HttpRule{response_body: ""}
    request_body = %{message: %{a: "b"}}

    assert request_body == Transcode.map_response_body(rule, request_body)
  end

  test "map_response_body/2 with HttpRule.response_body: <field>" do
    rule = %Google.Api.HttpRule{response_body: "message"}
    request_body = %{message: %{a: "b"}}

    assert %{a: "b"} == Transcode.map_response_body(rule, request_body)
  end

  # CVE-2026-48599 / GHSA-mwr4-5g34-j5cq regression tests:
  # Path bindings must never be overridden by query-string or body parameters.

  test "map_request/5 with empty body: path binding cannot be overridden by query string" do
    rule = %Google.Api.HttpRule{body: ""}
    request_body = %{}
    bindings = %{"latitude" => 1}
    qs = "latitude=999"

    assert {:ok, %Routeguide.Point{latitude: 1}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/5 with body '*': path binding cannot be overridden by body" do
    rule = %Google.Api.HttpRule{body: "*"}
    request_body = %{"latitude" => 999, "longitude" => 2}
    bindings = %{"latitude" => 1}
    qs = ""

    assert {:ok, %Routeguide.Point{latitude: 1, longitude: 2}} =
             Transcode.map_request(rule, request_body, bindings, qs, Routeguide.Point)
  end

  test "map_request/5 with named body field: path binding cannot be overridden by query string or body" do
    rule = %Google.Api.HttpRule{body: "location"}
    request_body = %{"latitude" => 1, "longitude" => 2}
    bindings = %{"name" => "legitimate"}

    assert {:ok, %Routeguide.Feature{name: "legitimate"}} =
             Transcode.map_request(
               rule,
               request_body,
               bindings,
               "name=attacker",
               Routeguide.Feature
             )
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
end

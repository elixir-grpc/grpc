defmodule GRPC.Server.Router.TemplateTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Router.Template

  describe "tokenize/2" do
    test "can tokenize simple paths" do
      assert [{:/, []}] = Template.tokenize("/")

      assert [{:/, []}, {:identifier, "v1", []}, {:/, []}, {:identifier, "messages", []}] =
               Template.tokenize("/v1/messages")
    end

    test "can tokenize simple paths with wildcards" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:identifier, "messages", []},
               {:/, []},
               {:*, []}
             ] == Template.tokenize("/v1/messages/*")
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
             ] == Template.tokenize("/v1/messages/{message_id}")
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
             ] == Template.tokenize("/v1/{name=messages}")
    end

    test "can tokenize variable sub-paths in bindings" do
      assert [
               {:/, []},
               {:identifier, "v1", []},
               {:/, []},
               {:"{", []},
               {:identifier, "name", []},
               {:=, []},
               {:identifier, "messages", []},
               {:/, []},
               {:*, []},
               {:"}", []}
             ] == Template.tokenize("/v1/{name=messages/*}")
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
             ] == Template.tokenize("/v1/messages/{message_id}/{sub.subfield}")
    end

    test "can tokenize single wildcard" do
      assert [{:/, []}, {:*, []}] == Template.tokenize("/*")
    end

    test "can tokenize multiple wildcards" do
      assert [
               {:/, []},
               {:*, []},
               {:/, []},
               {:*, []},
               {:*, []}
             ] == Template.tokenize("/*/**")
    end
  end

  describe "parse/3" do
    test "can parse simple paths" do
      assert [] ==
               "/"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse paths with identifiers" do
      assert ["v1", "messages"] ==
               "/v1/messages"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse paths with 'any'" do
      assert ["v1", "messages", {:_, []}] ==
               "/v1/messages/*"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse paths with 'catch all'" do
      assert ["v1", "messages", {:__, []}] ==
               "/v1/messages/**"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse simple bindings with variables" do
      assert ["v1", "messages", {:message_id, []}] ==
               "/v1/messages/{message_id}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse bindings with variable assignment" do
      assert ["v1", {:name, ["messages", {:_, []}]}] ==
               "/v1/{name=messages/*}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse bindings with variable assignment to any" do
      assert ["v1", {:name, [{:_, []}]}] ==
               "/v1/{name=*}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse multiple bindings with variable assignment" do
      assert ["v1", {:name, ["messages"]}, {:message_id, []}] ==
               "/v1/{name=messages}/{message_id}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse bindings with field paths" do
      assert ["v1", "messages", {:"sub.subfield", []}] ==
               "/v1/messages/{sub.subfield}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "supports deeper nested field path " do
      assert ["v1", "messages", {:"sub.nested.nested.nested", []}] ==
               "/v1/messages/{sub.nested.nested.nested}"
               |> Template.tokenize()
               |> Template.parse([])
    end

    test "can parse multiple-bindings with field paths " do
      assert ["v1", "messages", {:"first.subfield", []}, {:"second.subfield", []}] ==
               "/v1/messages/{first.subfield}/{second.subfield}"
               |> Template.tokenize()
               |> Template.parse([])
    end
  end
end

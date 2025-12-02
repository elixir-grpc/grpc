defmodule GRPC.Server.RouterTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Router

  describe "build_route/1" do
    test "returns a route with {http_method, path, match} based on the template string" do
      path = "/v1/messages/{message_id}"

      assert {:get, ^path, match} = Router.build_route(:get, "/v1/messages/{message_id}")
      assert ["v1", "messages", {:message_id, []}] = match
    end

    test "defaults to setting method to `:post` if no method was provided" do
      path = "/transcode.Messaging/GetMessage"

      assert {:post, ^path, match} = Router.build_route(path)
      assert ["transcode.Messaging", "GetMessage"] = match
    end

    test "returns a route with {http_method, path, match} based HttRule" do
      path = "/v1/messages/{message_id}"
      rule = build_simple_rule(:get, "/v1/messages/{message_id}")

      assert {:get, ^path, match} = Router.build_route(rule)
      assert ["v1", "messages", {:message_id, []}] = match
    end
  end

  describe "match/3" do
    test "with no segments" do
      assert {true, %{}} = Router.match("/", [])
    end

    test "with segments and no matchers" do
      refute Router.match("/foo", [])
    end

    test "with no bindings" do
      {_, _, match} = Router.build_route("/transcode.Messaging/GetMessage")

      assert {true, %{}} == Router.match("/transcode.Messaging/GetMessage", match)
      assert false == Router.match("/transcode.Messaging/GetMessages", match)
    end

    test "with simple bindings" do
      {_, _, match} = Router.build_route(:get, "/v1/{name}")

      assert {true, %{name: "messages"}} == Router.match("/v1/messages", match)
    end

    test "with nested bindings" do
      {_, _, match} = Router.build_route(:get, "/v1/{message.name}")

      assert {true, %{"message.name": "messages"}} == Router.match("/v1/messages", match)
    end

    test "with multiple bindings" do
      {_, _, match} =
        Router.build_route(:get, "/v1/users/{user_id}/messages/{message.message_id}")

      assert {true, %{user_id: "1", "message.message_id": "2"}} ==
               Router.match("/v1/users/1/messages/2", match)
    end

    test "with multiple sequential bindings" do
      {_, _, match} = Router.build_route("/v1/{a}/{b}/{c}")

      assert {true, %{a: "a", b: "b", c: "c"}} == Router.match("/v1/a/b/c", match)
    end

    test "with 'any'" do
      {_, _, match} = Router.build_route("/*")

      assert {true, %{}} == Router.match("/v1", match)
    end

    test "with 'any' assignment" do
      {_, _, match} = Router.build_route("/{a=*}")

      assert {true, %{a: "v1"}} == Router.match("/v1", match)
    end

    test "with 'catch all' assignment" do
      {_, _, match} = Router.build_route("/{a=**}")

      assert {true, %{a: "v1/messages"}} == Router.match("/v1/messages", match)
    end

    test "with 'any' and 'catch all'" do
      {_, _, match} = Router.build_route("/*/**")
      assert {true, %{}} == Router.match("/v1/foo/bar/baz", match)
    end

    test "with 'any' and 'catch all' assignment" do
      {_, _, match} = Router.build_route("/*/a/{b=c/*}/d/{e=**}")

      assert {true, %{b: "c/foo", e: "bar/baz/biz"}} ==
               Router.match("/v1/a/c/foo/d/bar/baz/biz", match)
    end

    test "with complex binding" do
      {_, _, match} = Router.build_route("/v1/{name=messages}")

      assert {true, %{name: "messages"}} == Router.match("/v1/messages", match)
      refute Router.match("/v1/should_not_match", match)
    end

    test "with complex binding and 'any'" do
      {_, _, match} = Router.build_route("/v1/{name=messages/*}")

      assert {true, %{name: "messages/12345"}} == Router.match("/v1/messages/12345", match)
      refute Router.match("/v1/should_not_match/12345", match)
    end

    test "with complex binding, wildcards and trailing route" do
      {_, _, match} = Router.build_route("/v1/{name=shelves/*/books/*}")

      assert {true, %{name: "shelves/example-shelf/books/example-book"}} ==
               Router.match("/v1/shelves/example-shelf/books/example-book", match)

      refute Router.match("/v1/shelves/example-shelf/not_books", match)
    end

    test "with complex binding, wildcards and suffix" do
      {_, _, match} = Router.build_route("/v1/{name=shelves/*/books/*}/suffix")

      assert {true, %{name: "shelves/example-shelf/books/example-book"}} ==
               Router.match(
                 "/v1/shelves/example-shelf/books/example-book/suffix",
                 match
               )

      refute Router.match(
               "/v1/shelves/example-shelf/books/example-book/another_suffix",
               match
             )
    end

    test "with mixed complex binding" do
      {_, _, match} = Router.build_route("/v1/{a=users/*}/messages/{message_id}/{c=books/*}")

      assert {true, %{a: "users/foobar", message_id: "1", c: "books/barbaz"}} ==
               Router.match("/v1/users/foobar/messages/1/books/barbaz", match)

      assert false == Router.match("/v1/users/1/books/barbaz", match)
    end

    test "with mixed sequential complex binding" do
      {_, _, match} = Router.build_route("/v1/{a=users/*}/{b=messages}/{c=books/*}")

      assert {true, %{a: "users/foobar", b: "messages", c: "books/barbaz"}} ==
               Router.match("/v1/users/foobar/messages/books/barbaz", match)

      refute Router.match("/v1/users/foobar/messages/book/books/barbaz", match)
    end
  end

  defp build_simple_rule(method, pattern) do
    %Google.Api.HttpRule{pattern: {method, pattern}}
  end
end

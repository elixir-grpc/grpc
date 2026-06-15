defmodule GRPC.Server.Adapters.Cowboy.RouterTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Adapters.Cowboy.Router

  describe "match/3" do
    test "with no_host" do
      assert {:error, :notfound, :host} = Router.match([], [], [])
    end

    test "with no bindings" do
      dispatch = make_dispatch("/transcode.Messaging/GetMessage")

      assert {:ok, Handler, [], %{}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/transcode.Messaging/GetMessage")

      assert {:error, :notfound, :path} == Router.match(dispatch, "localhost", "/unknown/path")
    end

    test "with simple bindings" do
      dispatch = make_dispatch("/v1/{name}")

      assert {:ok, Handler, [], %{name: "messages"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/messages")
    end

    test "with nested bindings" do
      dispatch = make_dispatch("/v1/{message.name}")

      assert {:ok, Handler, [], %{"message.name": "messages"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/messages")
    end

    test "with multiple bindings" do
      dispatch = make_dispatch("/v1/users/{user_id}/messages/{message.message_id}")

      assert {:ok, Handler, [], %{user_id: "1", "message.message_id": "2"}, :undefined,
              :undefined} ==
               Router.match(dispatch, "localhost", "/v1/users/1/messages/2")
    end

    test "with multiple sequential bindings" do
      dispatch = make_dispatch("/v1/{a}/{b}/{c}")

      assert {:ok, Handler, [], %{a: "a", b: "b", c: "c"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/a/b/c")
    end

    test "with any " do
      dispatch = make_dispatch("/*")

      assert {:ok, Handler, [], %{}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1")
    end

    test "with 'any' assignment" do
      dispatch = make_dispatch("/{a=*}")

      assert {:ok, Handler, [], %{a: "v1"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1")
    end

    test "with 'catch all' assignment" do
      dispatch = make_dispatch("/{a=**}")

      assert {:ok, Handler, [], %{a: "v1/messages"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/messages")
    end

    test "with 'any' and 'catch all'" do
      dispatch = make_dispatch("/*/**")

      assert {:ok, Handler, [], %{}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/foo/bar/baz")
    end

    test "with 'any' and 'catch all' assignment" do
      dispatch = make_dispatch("/*/a/{b=c/*}/d/{e=**}")

      assert {:ok, Handler, [], %{b: "c/foo", e: "bar/baz/biz"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/a/c/foo/d/bar/baz/biz")
    end

    test "with complex binding" do
      dispatch = make_dispatch("/v1/{name=messages}")

      assert {:ok, Handler, [], %{name: "messages"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/messages")

      assert {:error, :notfound, :path} ==
               Router.match(dispatch, "localhost", "/v1/should_not_match")
    end

    test "with complex binding and 'any'" do
      dispatch = make_dispatch("/v1/{name=messages/*}")

      assert {:ok, Handler, [], %{name: "messages/12345"}, :undefined, :undefined} ==
               Router.match(dispatch, "localhost", "/v1/messages/12345")

      assert {:error, :notfound, :path} ==
               Router.match(dispatch, "localhost", "/v1/should_not_match/12345")
    end

    test "with complex binding, wildcards and trailing route" do
      dispatch = make_dispatch("/v1/{name=shelves/*/books/*}")

      assert {:ok, Handler, [], %{name: "shelves/example-shelf/books/example-book"}, :undefined,
              :undefined} ==
               Router.match(dispatch, "localhost", "/v1/shelves/example-shelf/books/example-book")

      assert {:error, :notfound, :path} ==
               Router.match(dispatch, "localhost", "/v1/shelves/example-shelf/not_books")
    end

    test "with complex binding, wildcards and suffix" do
      dispatch = make_dispatch("/v1/{name=shelves/*/books/*}/suffix")

      assert {:ok, Handler, [], %{name: "shelves/example-shelf/books/example-book"}, :undefined,
              :undefined} ==
               Router.match(
                 dispatch,
                 "localhost",
                 "/v1/shelves/example-shelf/books/example-book/suffix"
               )

      assert {:error, :notfound, :path} ==
               Router.match(
                 dispatch,
                 "localhost",
                 "/v1/shelves/example-shelf/books/example-book/another_suffix"
               )
    end

    test "with mixed complex binding" do
      dispatch = make_dispatch("/v1/{a=users/*}/messages/{message_id}/{c=books/*}")

      assert {:ok, Handler, [], %{a: "users/foobar", message_id: "1", c: "books/barbaz"},
              :undefined,
              :undefined} ==
               Router.match(dispatch, "localhost", "/v1/users/foobar/messages/1/books/barbaz")

      assert {:error, :notfound, :path} ==
               Router.match(dispatch, "localhost", "/v1/users/1/books/barbaz")
    end

    test "with mixed sequential complex binding" do
      dispatch = make_dispatch("/v1/{a=users/*}/{b=messages}/{c=books/*}")

      assert {:ok, Handler, [], %{a: "users/foobar", b: "messages", c: "books/barbaz"},
              :undefined,
              :undefined} ==
               Router.match(dispatch, "localhost", "/v1/users/foobar/messages/books/barbaz")

      assert {:error, :notfound, :path} ==
               Router.match(dispatch, "localhost", "/v1/users/foobar/messages/book/books/barbaz")
    end
  end

  defp make_dispatch(path) do
    {_method, _, match} = GRPC.Server.Router.build_route(path)

    [
      {:_, [],
       [
         {match, [], Handler, []}
       ]}
    ]
  end
end

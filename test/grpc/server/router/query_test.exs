defmodule GRPC.Server.Router.QueryTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Router.Query

  test "`a=b&c=d` should be decoded as a map" do
    assert %{"a" => "b", "c" => "d"} == Query.decode("a=b&c=d")
  end

  test "`param=A&param=B` should be decoded as a list" do
    assert %{"param" => ["A", "B"]} == Query.decode("param=A&param=B")
  end

  test "`root.a=A&root.b=B` should be decoded as a nested map" do
    assert %{"root" => %{"a" => "A", "b" => "B"}} == Query.decode("root.a=A&root.b=B")
  end

  test "`root.a=A&root.a=B` should be decoded as a nested map with a list" do
    assert %{"root" => %{"a" => ["A", "B"]}} == Query.decode("root.a=A&root.a=B")
  end

  test "deeply nested map should be decoded" do
    assert %{"root" => %{"a" => %{"b" => %{"c" => %{"d" => "A"}}}, "b" => "B"}, "c" => "C"} ==
             Query.decode("root.a.b.c.d=A&root.b=B&c=C")
  end

  test "pairs without value are decoded as `\"\"`" do
    assert %{"param" => "", "a" => "A"} ==
             Query.decode("param=&a=A")
  end
end

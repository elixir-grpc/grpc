defmodule BenchmarkgTest do
  use ExUnit.Case
  doctest Benchmarkg

  test "greets the world" do
    assert Benchmarkg.hello() == :world
  end
end

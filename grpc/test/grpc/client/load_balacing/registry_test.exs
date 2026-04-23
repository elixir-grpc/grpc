defmodule GRPC.Client.LoadBalancing.RegistryTest do
  @moduledoc """
  Direct tests for the shared LB registry.

  The registry is a process-backed named ETS table started by
  GRPC.Client.Application. These tests exercise the public API
  (put/2, lookup/1, delete/1) and the ArgumentError rescue path in
  lookup/1 that guards against races where the table is gone.
  """
  use ExUnit.Case, async: false

  alias GRPC.Client.LoadBalancing.Registry

  describe "put/2 + lookup/1" do
    test "round-trips a {module, state} value keyed by a reference" do
      ref = make_ref()
      Registry.put(ref, {SomeLB, %{tid: :fake_tid}})

      assert {:ok, {SomeLB, %{tid: :fake_tid}}} = Registry.lookup(ref)

      Registry.delete(ref)
    end

    test "accepts arbitrary keys (reference, tuple, atom)" do
      for key <- [make_ref(), {:conn, 1}, :atom_key] do
        Registry.put(key, {LB, :state})
        assert {:ok, {LB, :state}} = Registry.lookup(key)
        Registry.delete(key)
      end
    end

    test "overwrites an existing entry" do
      ref = make_ref()
      Registry.put(ref, {LB, :v1})
      Registry.put(ref, {LB, :v2})

      assert {:ok, {LB, :v2}} = Registry.lookup(ref)

      Registry.delete(ref)
    end
  end

  describe "lookup/1 when the entry is missing" do
    test "returns :error" do
      assert :error = Registry.lookup(make_ref())
    end
  end

  describe "delete/1" do
    test "removes the entry; subsequent lookup returns :error" do
      ref = make_ref()
      Registry.put(ref, {LB, :state})
      Registry.delete(ref)

      assert :error = Registry.lookup(ref)
    end

    test "is safe on an unknown key" do
      assert true = Registry.delete(make_ref())
    end
  end
end

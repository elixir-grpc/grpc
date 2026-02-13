defmodule GRPC.ServiceCollisionTest do
  use ExUnit.Case, async: true

  describe "detect_name_collisions/1" do
    test "detects collision between snake_case and CamelCase RPCs" do
      rpc_calls = [
        {:process_data, {:req, false}, {:res, false}, %{}},
        {:ProcessData, {:req, false}, {:res, false}, %{}}
      ]

      {annotated_calls, collision_map} = GRPC.Service.detect_name_collisions(rpc_calls)

      # Both RPCs should be marked with collision flag
      assert length(annotated_calls) == 2

      assert Enum.all?(annotated_calls, fn {_, _, _, opts} ->
               opts[:__name_collision__] == true
             end)

      # "process_data" should be in the collision map with both original names
      assert Map.has_key?(collision_map, "process_data")
      assert :process_data in collision_map["process_data"]
      assert :ProcessData in collision_map["process_data"]
    end

    test "detects multiple colliding groups" do
      rpc_calls = [
        {:GetFoo, {:req, false}, {:res, false}, %{}},
        {:GetFOO, {:req, false}, {:res, false}, %{}},
        {:SetBar, {:req, false}, {:res, false}, %{}},
        {:SetBAR, {:req, false}, {:res, false}, %{}}
      ]

      {annotated_calls, collision_map} = GRPC.Service.detect_name_collisions(rpc_calls)

      # All four RPCs should be marked with collision flag
      assert length(annotated_calls) == 4

      assert Enum.all?(annotated_calls, fn {_, _, _, opts} ->
               opts[:__name_collision__] == true
             end)

      # Both "get_foo" and "set_bar" should be in the collision map
      assert Map.has_key?(collision_map, "get_foo")
      assert Map.has_key?(collision_map, "set_bar")
      assert length(collision_map["get_foo"]) == 2
      assert length(collision_map["set_bar"]) == 2
    end

    test "does not mark non-colliding RPCs" do
      rpc_calls = [
        {:GetUser, {:req, false}, {:res, false}, %{}},
        {:CreateUser, {:req, false}, {:res, false}, %{}},
        {:DeleteUser, {:req, false}, {:res, false}, %{}}
      ]

      {annotated_calls, collision_map} = GRPC.Service.detect_name_collisions(rpc_calls)

      # None should be marked with collision flag
      assert Enum.all?(annotated_calls, fn {_, _, _, opts} ->
               opts[:__name_collision__] != true
             end)

      # No collisions
      assert collision_map == %{}
    end

    test "handles mix of colliding and non-colliding RPCs" do
      rpc_calls = [
        {:GetUser, {:req, false}, {:res, false}, %{}},
        {:process_data, {:req, false}, {:res, false}, %{}},
        {:ProcessData, {:req, false}, {:res, false}, %{}},
        {:DeleteUser, {:req, false}, {:res, false}, %{}}
      ]

      {annotated_calls, collision_map} = GRPC.Service.detect_name_collisions(rpc_calls)

      # Only process_data and ProcessData should be marked
      marked_calls =
        Enum.filter(annotated_calls, fn {_, _, _, opts} -> opts[:__name_collision__] == true end)

      assert length(marked_calls) == 2

      marked_names = Enum.map(marked_calls, fn {name, _, _, _} -> name end)
      assert :process_data in marked_names
      assert :ProcessData in marked_names

      # Only "process_data" should be in collision map
      assert Map.keys(collision_map) == ["process_data"]
      assert length(collision_map["process_data"]) == 2
    end

    test "preserves existing options in annotated calls" do
      rpc_calls = [
        {:GetFoo, {:req, false}, {:res, false}, %{existing: :option}},
        {:GetFOO, {:req, false}, {:res, false}, %{another: :value}}
      ]

      {annotated_calls, _collisions} = GRPC.Service.detect_name_collisions(rpc_calls)

      # Check that existing options are preserved
      [{_, _, _, opts1}, {_, _, _, opts2}] = annotated_calls
      assert opts1[:existing] == :option
      assert opts1[:__name_collision__] == true
      assert opts2[:another] == :value
      assert opts2[:__name_collision__] == true
    end
  end
end

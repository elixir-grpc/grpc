defmodule GRPC.StubCollisionTest do
  use ExUnit.Case, async: true

  test "collision detection properly detects colliding RPC names" do
    # The CollisionTest.CollisionService has:
    # - :process_data (unary, colliding with ProcessData)
    # - :ProcessData (unary, colliding with process_data)
    # - :GetUser (unary, non-colliding)

    service = CollisionTest.CollisionService.Service
    collision_map = service.__rpc_name_collisions__()

    # Should detect one collision group
    assert map_size(collision_map) == 1

    # "process_data" should be in the collision map with both original names
    assert Map.has_key?(collision_map, "process_data")
    assert :process_data in collision_map["process_data"]
    assert :ProcessData in collision_map["process_data"]
  end

  test "convenience functions are only generated for non-colliding RPCs" do
    # The CollisionTest.CollisionService has:
    # - :process_data (colliding) → should NOT have convenience function
    # - :ProcessData (colliding) → should NOT have convenience function
    # - :GetUser (non-colliding) → SHOULD have convenience function

    stub = CollisionTest.CollisionService.Stub

    # Get all exported functions
    functions = stub.__info__(:functions)
    function_names = Enum.map(functions, fn {name, _arity} -> name end) |> MapSet.new()

    # Non-colliding RPC should have convenience function
    assert :get_user in function_names,
      "Expected get_user convenience function to be generated for non-colliding RPC :GetUser"

    # Colliding RPCs should NOT have convenience functions
    refute :process_data in function_names,
      "Expected NO process_data convenience function due to collision with :ProcessData"

    # All RPCs should be accessible via call/4 (they're all unary)
    # We can't easily check this without inspecting the function body,
    # but we can verify that call/4 clauses exist by checking the function is exported
    assert function_exported?(stub, :call, 4),
      "Expected call/4 to be exported for unary RPC calls"
  end
end

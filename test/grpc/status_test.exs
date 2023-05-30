defmodule GRPC.StatusTest do
	use ExUnit.Case

	alias GRPC.Status

  @status_grpc_codes %{
    ok: 0,
    cancelled: 1,
    unknown: 2,
    invalid_argument: 3,
    deadline_exceeded: 4,
    not_found: 5,
    already_exists: 6,
    permission_denied: 7,
    resource_exhausted: 8,
    failed_precondition: 9,
    aborted: 10,
    out_of_range: 11,
    unimplemented: 12,
    internal: 13,
    unavailable: 14,
    data_loss: 15,
    unauthenticated: 16
  }

  test "match an error atom status to a gRPC status defined code" do
    for {function, expected_value} <- @status_grpc_codes, do:
      assert expected_value == apply(Status, function, [])
  end
end

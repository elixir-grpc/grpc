defmodule GRPC.StatusTest do
  use GRPC.DataCase, async: true

  alias GRPC.Status

  test "match an error atom status to a gRPC status defined code" do
    for {function, expected_code, _expected_message} <- grpc_status_codes_messages_factory(),
        do: assert(expected_code == apply(Status, function, []))
  end
end

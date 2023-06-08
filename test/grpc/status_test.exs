defmodule GRPC.StatusTest do
  use GRPC.DataCase, async: true

  alias GRPC.Status

  @status_codes_metadata [
    {:ok, 0, nil},
    {:cancelled, 1, "The operation was cancelled (typically by the caller)"},
    {:unknown, 2, "Unknown error"},
    {:invalid_argument, 3, "Client specified an invalid argument"},
    {:deadline_exceeded, 4, "Deadline expired before operation could complete"},
    {:not_found, 5, "Some requested entity (e.g., file or directory) was not found"},
    {:already_exists, 6,
     "Some entity that we attempted to create (e.g., file or directory) already exists"},
    {:permission_denied, 7,
     "The caller does not have permission to execute the specified operation"},
    {:resource_exhausted, 8, "Some resource has been exhausted"},
    {:failed_precondition, 9,
     "Operation was rejected because the system is not in a state required for the operation's execution"},
    {:aborted, 10, "The operation was aborted"},
    {:out_of_range, 11, "Operation was attempted past the valid range"},
    {:unimplemented, 12, "Operation is not implemented or not supported/enabled in this service"},
    {:internal, 13, "Internal errors"},
    {:unavailable, 14, "The service is currently unavailable"},
    {:data_loss, 15, "Unrecoverable data loss or corruption"},
    {:unauthenticated, 16,
     "The request does not have valid authentication credentials for the operation"}
  ]

  for {function, expected_code, expected_message} <- @status_codes_metadata do
    test "function #{function} returns expected code" do
      assert unquote(expected_code) == Status.unquote(function)()
    end

    test "code #{expected_code} returns the expected message" do
      assert unquote(expected_message) == Status.status_message(unquote(expected_code))
    end

    test "code #{expected_code} returns the expected code_name" do
      code_name =
        case unquote(function) do
          :ok -> "OK"
          :cancelled -> "Canceled"
          function -> function |> to_string() |> Macro.camelize()
        end

      assert code_name == Status.code_name(unquote(expected_code))
    end
  end
end

defmodule GRPC.Status do
  @moduledoc """
  A collection of gRPC statuses.

  |  Code  |         Status           |                                            Status Message                                           |
  |-------:|--------------------------|-----------------------------------------------------------------------------------------------------|
  |   `0`  |  `:ok`                   |  Success                                                                                            |
  |   `1`  |  `:cancelled`            |  The operation was cancelled (typically by the caller)                                              |
  |   `2`  |  `:unknown`              |  Unknown error                                                                                      |
  |   `3`  |  `:invalid_argument`     |  Client specified an invalid argument                                                               |
  |   `4`  |  `:deadline_exceeded`    |  Deadline expired before operation could complete                                                   |
  |   `5`  |  `:not_found`            |  Some requested entity (e.g., file or directory) was not found                                      |
  |   `6`  |  `:already_exists`       |  Some entity that we attempted to create (e.g., file or directory) already exists                   |
  |   `7`  |  `:permission_denied`    |  The caller does not have permission to execute the specified operation                             |
  |   `8`  |  `:resource_exhausted`   |  Some resource has been exhausted                                                                   |
  |   `9`  |  `:failed_precondition`  |  Operation was rejected because the system is not in a state required for the operation's execution |
  |  `10`  |  `:aborted`              |  The operation was aborted                                                                          |
  |  `11`  |  `:out_of_range`         |  Operation was attempted past the valid range                                                       |
  |  `12`  |  `:unimplemented`        |  Operation is not implemented or not supported/enabled in this service                              |
  |  `13`  |  `:internal`             |  Internal errors                                                                                    |
  |  `14`  |  `:unavailable`          |  The service is currently unavailable                                                               |
  |  `15`  |  `:data_loss`            |  Unrecoverable data loss or corruption                                                              |
  |  `16`  |  `:unauthenticated`      |  The request does not have valid authentication credentials for the operation                       |

  For more details, please refer to the [official gRPC docs](https://github.com/grpc/grpc/blob/master/doc/statuscodes.md)
  """

  @type t :: non_neg_integer

  @doc """
  Not an error; returned on success.
  """
  @spec ok :: t
  def ok, do: 0

  @doc """
  The operation was cancelled (typically by the caller).
  """
  @spec cancelled() :: t()
  def cancelled, do: 1

  @doc """
  Unknown error.

  An example of where this error may be returned is
  if a Status value received from another address space belongs to
  an error-space that is not known in this address space. Also
  errors raised by APIs that do not return enough error information
  may be converted to this error.
  """
  @spec unknown :: t
  def unknown, do: 2

  @doc """
  Client specified an invalid argument.

  Note that this differs from FAILED_PRECONDITION.
  INVALID_ARGUMENT indicates arguments that are problematic regardless of
  the state of the system (e.g., a malformed file name).
  """
  @spec invalid_argument :: t
  def invalid_argument, do: 3

  @doc """
  Deadline expired before operation could complete.

  For operations that change the state of the system, this error may be returned
  even if the operation has completed successfully. For example, a
  successful response from a server could have been delayed long
  enough for the deadline to expire.
  """
  @spec deadline_exceeded :: t
  def deadline_exceeded, do: 4

  @doc """
  Some requested entity (e.g., file or directory) was not found.
  """
  @spec not_found :: t
  def not_found, do: 5

  @doc """
  Some entity that we attempted to create (e.g., file or directory) already exists.
  """
  @spec already_exists :: t
  def already_exists, do: 6

  @doc """
  The caller does not have permission to execute the specified
  operation.

  PERMISSION_DENIED must not be used for rejections
  caused by exhausting some resource (use RESOURCE_EXHAUSTED
  instead for those errors).  PERMISSION_DENIED must not be
  used if the caller can not be identified (use UNAUTHENTICATED
  instead for those errors).
  """
  @spec permission_denied :: t
  def permission_denied, do: 7

  @doc """
  Some resource has been exhausted, perhaps a per-user quota, or
  perhaps the entire file system is out of space.
  """
  @spec resource_exhausted :: t
  def resource_exhausted, do: 8

  @doc """
  Operation was rejected because the system is not in a state
  required for the operation's execution.

  For example, directory to be deleted may be non-empty,
  an rmdir operation is applied to a non-directory, etc.
  """
  @spec failed_precondition :: t
  def failed_precondition, do: 9

  @doc """
  The operation was aborted.

  Typically due to a concurrency issue like sequencer check failures,
  transaction aborts, etc.
  """
  @spec aborted() :: t()
  def aborted, do: 10

  @doc """
  Operation was attempted past the valid range.

  E.g., seeking or reading past end of file.
  """
  @spec out_of_range :: t
  def out_of_range, do: 11

  @doc """
  Operation is not implemented or not supported/enabled in this service.
  """
  @spec unimplemented :: t
  def unimplemented, do: 12

  @doc """
  Internal errors.

  Means some invariants expected by underlying system has been broken.
  If you see one of these errors, something is very broken.
  """
  @spec internal :: t
  def internal, do: 13

  @doc """
  The service is currently unavailable.

  This is a most likely a transient condition and may be corrected by retrying with
  a backoff.
  """
  @spec unavailable :: t
  def unavailable, do: 14

  @doc """
  Unrecoverable data loss or corruption.
  """
  @spec data_loss :: t
  def data_loss, do: 15

  @doc """
  The request does not have valid authentication credentials for the operation.
  """
  @spec unauthenticated :: t
  def unauthenticated, do: 16

  @spec code_name(t()) :: binary()
  def code_name(0), do: "OK"
  def code_name(1), do: "Canceled"
  def code_name(2), do: "Unknown"
  def code_name(3), do: "InvalidArgument"
  def code_name(4), do: "DeadlineExceeded"
  def code_name(5), do: "NotFound"
  def code_name(6), do: "AlreadyExists"
  def code_name(7), do: "PermissionDenied"
  def code_name(8), do: "ResourceExhausted"
  def code_name(9), do: "FailedPrecondition"
  def code_name(10), do: "Aborted"
  def code_name(11), do: "OutOfRange"
  def code_name(12), do: "Unimplemented"
  def code_name(13), do: "Internal"
  def code_name(14), do: "Unavailable"
  def code_name(15), do: "DataLoss"
  def code_name(16), do: "Unauthenticated"

  @spec http_code(t()) :: t()
  def http_code(0), do: 200
  def http_code(1), do: 400
  def http_code(2), do: 500
  def http_code(3), do: 400
  def http_code(4), do: 504
  def http_code(5), do: 404
  def http_code(6), do: 409
  def http_code(7), do: 403
  def http_code(8), do: 429
  def http_code(9), do: 412
  def http_code(10), do: 409
  def http_code(11), do: 400
  def http_code(12), do: 501
  def http_code(13), do: 500
  def http_code(14), do: 503
  def http_code(15), do: 500
  def http_code(16), do: 401

  @spec status_message(t()) :: String.t() | nil
  def status_message(0), do: nil
  def status_message(1), do: "The operation was cancelled (typically by the caller)"
  def status_message(2), do: "Unknown error"
  def status_message(3), do: "Client specified an invalid argument"
  def status_message(4), do: "Deadline expired before operation could complete"
  def status_message(5), do: "Some requested entity (e.g., file or directory) was not found"

  def status_message(6),
    do: "Some entity that we attempted to create (e.g., file or directory) already exists"

  def status_message(7),
    do: "The caller does not have permission to execute the specified operation"

  def status_message(8), do: "Some resource has been exhausted"

  def status_message(9),
    do:
      "Operation was rejected because the system is not in a state required for the operation's execution"

  def status_message(10), do: "The operation was aborted"
  def status_message(11), do: "Operation was attempted past the valid range"

  def status_message(12),
    do: "Operation is not implemented or not supported/enabled in this service"

  def status_message(13), do: "Internal errors"
  def status_message(14), do: "The service is currently unavailable"
  def status_message(15), do: "Unrecoverable data loss or corruption"

  def status_message(16),
    do: "The request does not have valid authentication credentials for the operation"
end

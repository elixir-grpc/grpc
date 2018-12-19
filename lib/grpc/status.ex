defmodule GRPC.Status do
  @moduledoc """
  Collection of gRPC status.

  Please refer to https://github.com/grpc/grpc/blob/master/include/grpc/impl/codegen/status.h
  """

  @type t :: non_neg_integer

  @doc """
  Not an error; returned on success.
  """
  def ok, do: 0

  @doc """
  The operation was cancelled (typically by the caller).
  """
  def cancelled, do: 1

  @doc """
  Unknown error.

  An example of where this error may be returned is
  if a Status value received from another address space belongs to
  an error-space that is not known in this address space.  Also
  errors raised by APIs that do not return enough error information
  may be converted to this error.
  """
  def unknown, do: 2

  @doc """
  Client specified an invalid argument.

  Note that this differs from FAILED_PRECONDITION.
  INVALID_ARGUMENT indicates arguments that are problematic regardless of
  the state of the system (e.g., a malformed file name).
  """
  def invalid_argument, do: 3

  @doc """
  Deadline expired before operation could complete.

  For operations that change the state of the system, this error may be returned
  even if the operation has completed successfully. For example, a
  successful response from a server could have been delayed long
  enough for the deadline to expire.
  """
  def deadline_exceeded, do: 4

  @doc """
  Some requested entity (e.g., file or directory) was not found.
  """
  def not_found, do: 5

  @doc """
  Some entity that we attempted to create (e.g., file or directory) already exists.
  """
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
  def permission_denied, do: 7

  @doc """
  Some resource has been exhausted, perhaps a per-user quota, or
  perhaps the entire file system is out of space.
  """
  def resource_exhausted, do: 8

  @doc """
  Operation was rejected because the system is not in a state
  required for the operation's execution.

  For example, directory to be deleted may be non-empty,
  an rmdir operation is applied to a non-directory, etc.
  """
  def failed_precondition, do: 9

  @doc """
  The operation was aborted.

  Typically due to a concurrency issue like sequencer check failures,
  transaction aborts, etc.
  """
  def aborted, do: 10

  @doc """
  Operation was attempted past the valid range.

  E.g., seeking or reading past end of file.
  """
  def out_of_range, do: 11

  @doc """
  Operation is not implemented or not supported/enabled in this service.
  """
  def unimplemented, do: 12

  @doc """
  Internal errors.

  Means some invariants expected by underlying system has been broken.
  If you see one of these errors, something is very broken.
  """
  def internal, do: 13

  @doc """
  The service is currently unavailable.

  This is a most likely a transient condition and may be corrected by retrying with
  a backoff.
  """
  def unavailable, do: 14

  @doc """
  Unrecoverable data loss or corruption.
  """
  def data_loss, do: 15

  @doc """
  The request does not have valid authentication credentials for the operation.
  """
  def unauthenticated, do: 16

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
end

defmodule GRPC.Codec do
  @moduledoc """
  Contains code to serialize a deserialize the message.
  """

  # Content subtype is a suffix after "application/grpc+" such as "proto".
  @callback content_subtype() :: String.t()
  @callback encode(any) :: binary
  @callback decode(any, atom) :: any
end

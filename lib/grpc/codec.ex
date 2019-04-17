defmodule GRPC.Codec do
  @moduledoc """
  Contains code to serialize a deserialize the message.
  """

  @callback content_type() :: String.t()
  @callback encode(any) :: binary
  @callback decode(any, atom) :: any
end

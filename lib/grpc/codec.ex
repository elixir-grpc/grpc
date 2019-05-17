defmodule GRPC.Codec do
  @moduledoc """
  Contains code to serialize a deserialize the message.
  """

  @doc ~s(Name is identity of the codec, which will be suffix after content-type "application/grpc+" such as "proto".)
  @callback name() :: String.t()
  @callback encode(any) :: binary
  @callback decode(any, module :: atom) :: any
end

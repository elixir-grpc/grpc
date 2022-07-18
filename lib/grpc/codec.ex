defmodule GRPC.Codec do
  @moduledoc """
  Contains code to serialize a deserialize the message.
  """

  @doc ~s(Name is identity of the codec, which will be suffix after content-type "application/grpc+" such as "proto".)
  @callback name() :: String.t()
  @callback encode(any) :: binary
  @callback decode(any, module :: atom) :: any

  @callback unpack_from_channel(binary) :: binary
  @callback pack_for_channel(binary) :: binary
  @optional_callbacks unpack_from_channel: 1, pack_for_channel: 1
end

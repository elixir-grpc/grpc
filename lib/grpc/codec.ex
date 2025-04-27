defmodule GRPC.Codec do
  @moduledoc """
  Contains code to serialize a deserialize the message.
  """

  @doc ~s(Name is identity of the codec, which will be suffix after content-type "application/grpc+" such as "proto".)
  @callback name() :: String.t()
  @callback encode(any) :: iodata()
  @callback decode(any, module :: atom) :: any

  @doc """
  This function is invoked before the gRPC payload is transformed into a protobuf message whenever it is defined.

  This can be used to apply a transform over the gRPC message before decoding it. For instance grpc-web using the `application/grpc-web-text`
  content type requires the message to be Base64-encoded, so a server receving messages using grpc-web-text will be required to
  do a Base64 decode on the payload before decoding the gRPC message.
  """
  @callback unpack_from_channel(binary) :: binary

  @doc """
  This function is invoked whenever it is defined after the protobuf message has been transformed into a gRPC payload.

  This can be used to apply a transform over the gRPC message before sending it.
  For instance grpc-web using the `application/grpc-web-text` content type requires the message to be Base64-encoded, so a server sending messages using grpc-web-text will be required to
  do a Base64 encode on the payload before sending the gRPC message.
  """
  @callback pack_for_channel(iodata()) :: binary
  @optional_callbacks unpack_from_channel: 1, pack_for_channel: 1
end

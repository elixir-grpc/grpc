defmodule Protobuf.DecodeError do
  @moduledoc """
  An error for when decoding a Protobuf message fails.
  """
  defexception message: "something wrong when decoding"
end

defmodule Protobuf.EncodeError do
  @moduledoc """
  An error for when encoding a Protobuf message fails.
  """
  defexception message: "something wrong when encoding"
end

defmodule Protobuf.InvalidError do
  defexception [:message]
end

defmodule Protobuf.ExtensionNotFound do
  defexception message: "extension for the field is not found"
end

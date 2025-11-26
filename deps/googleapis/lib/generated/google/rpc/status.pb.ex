defmodule Google.Rpc.Status do
  @moduledoc """
  The `Status` type defines a logical error model that is suitable for
  different programming environments, including REST APIs and RPC APIs. It is
  used by [gRPC](https://github.com/grpc). Each `Status` message contains
  three pieces of data: error code, error message, and error details.

  You can find out more about this error model and how to work with it in the
  [API Design Guide](https://cloud.google.com/apis/design/errors).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :code, 1, type: :int32
  field :message, 2, type: :string
  field :details, 3, repeated: true, type: Google.Protobuf.Any
end

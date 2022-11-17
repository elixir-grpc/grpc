defmodule Grpc.Testing.ByteBufferParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :req_size, 1, type: :int32, json_name: "reqSize"
  field :resp_size, 2, type: :int32, json_name: "respSize"
end

defmodule Grpc.Testing.SimpleProtoParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :req_size, 1, type: :int32, json_name: "reqSize"
  field :resp_size, 2, type: :int32, json_name: "respSize"
end

defmodule Grpc.Testing.ComplexProtoParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3
end

defmodule Grpc.Testing.PayloadConfig do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :payload, 0

  field :bytebuf_params, 1,
    type: Grpc.Testing.ByteBufferParams,
    json_name: "bytebufParams",
    oneof: 0

  field :simple_params, 2,
    type: Grpc.Testing.SimpleProtoParams,
    json_name: "simpleParams",
    oneof: 0

  field :complex_params, 3,
    type: Grpc.Testing.ComplexProtoParams,
    json_name: "complexParams",
    oneof: 0
end
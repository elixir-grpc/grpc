defmodule Grpc.Testing.ByteBufferParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          req_size: integer,
          resp_size: integer
        }
  defstruct [:req_size, :resp_size]

  field :req_size, 1, type: :int32
  field :resp_size, 2, type: :int32
end

defmodule Grpc.Testing.SimpleProtoParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          req_size: integer,
          resp_size: integer
        }
  defstruct [:req_size, :resp_size]

  field :req_size, 1, type: :int32
  field :resp_size, 2, type: :int32
end

defmodule Grpc.Testing.ComplexProtoParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  defstruct []
end

defmodule Grpc.Testing.PayloadConfig do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          payload: {atom, any}
        }
  defstruct [:payload]

  oneof :payload, 0
  field :bytebuf_params, 1, type: Grpc.Testing.ByteBufferParams, oneof: 0
  field :simple_params, 2, type: Grpc.Testing.SimpleProtoParams, oneof: 0
  field :complex_params, 3, type: Grpc.Testing.ComplexProtoParams, oneof: 0
end

defmodule Google.Bytestream.ReadRequest do
  @moduledoc """
  Request object for ByteStream.Read.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :resource_name, 1, type: :string, json_name: "resourceName"
  field :read_offset, 2, type: :int64, json_name: "readOffset"
  field :read_limit, 3, type: :int64, json_name: "readLimit"
end

defmodule Google.Bytestream.ReadResponse do
  @moduledoc """
  Response object for ByteStream.Read.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :data, 10, type: :bytes
end

defmodule Google.Bytestream.WriteRequest do
  @moduledoc """
  Request object for ByteStream.Write.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :resource_name, 1, type: :string, json_name: "resourceName"
  field :write_offset, 2, type: :int64, json_name: "writeOffset"
  field :finish_write, 3, type: :bool, json_name: "finishWrite"
  field :data, 10, type: :bytes
end

defmodule Google.Bytestream.WriteResponse do
  @moduledoc """
  Response object for ByteStream.Write.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :committed_size, 1, type: :int64, json_name: "committedSize"
end

defmodule Google.Bytestream.QueryWriteStatusRequest do
  @moduledoc """
  Request object for ByteStream.QueryWriteStatus.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :resource_name, 1, type: :string, json_name: "resourceName"
end

defmodule Google.Bytestream.QueryWriteStatusResponse do
  @moduledoc """
  Response object for ByteStream.QueryWriteStatus.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :committed_size, 1, type: :int64, json_name: "committedSize"
  field :complete, 2, type: :bool
end

defmodule Grpc.Core.Bucket do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :start, 1, type: :double
  field :count, 2, type: :uint64
end

defmodule Grpc.Core.Histogram do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :buckets, 1, repeated: true, type: Grpc.Core.Bucket
end

defmodule Grpc.Core.Metric do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :value, 0

  field :name, 1, type: :string
  field :count, 10, type: :uint64, oneof: 0
  field :histogram, 11, type: Grpc.Core.Histogram, oneof: 0
end

defmodule Grpc.Core.Stats do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :metrics, 1, repeated: true, type: Grpc.Core.Metric
end
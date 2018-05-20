defmodule Grpc.Core.Bucket do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          start: float,
          count: non_neg_integer
        }
  defstruct [:start, :count]

  field :start, 1, type: :double
  field :count, 2, type: :uint64
end

defmodule Grpc.Core.Histogram do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          buckets: [Grpc.Core.Bucket.t()]
        }
  defstruct [:buckets]

  field :buckets, 1, repeated: true, type: Grpc.Core.Bucket
end

defmodule Grpc.Core.Metric do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: {atom, any},
          name: String.t()
        }
  defstruct [:value, :name]

  oneof :value, 0
  field :name, 1, type: :string
  field :count, 10, type: :uint64, oneof: 0
  field :histogram, 11, type: Grpc.Core.Histogram, oneof: 0
end

defmodule Grpc.Core.Stats do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          metrics: [Grpc.Core.Metric.t()]
        }
  defstruct [:metrics]

  field :metrics, 1, repeated: true, type: Grpc.Core.Metric
end

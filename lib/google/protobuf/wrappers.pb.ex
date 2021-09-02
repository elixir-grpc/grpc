defmodule Google.Protobuf.DoubleValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: float | :infinity | :negative_infinity | :nan
        }

  defstruct [:value]

  field :value, 1, type: :double
end

defmodule Google.Protobuf.FloatValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: float | :infinity | :negative_infinity | :nan
        }

  defstruct [:value]

  field :value, 1, type: :float
end

defmodule Google.Protobuf.Int64Value do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: integer
        }

  defstruct [:value]

  field :value, 1, type: :int64
end

defmodule Google.Protobuf.UInt64Value do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: non_neg_integer
        }

  defstruct [:value]

  field :value, 1, type: :uint64
end

defmodule Google.Protobuf.Int32Value do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: integer
        }

  defstruct [:value]

  field :value, 1, type: :int32
end

defmodule Google.Protobuf.UInt32Value do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: non_neg_integer
        }

  defstruct [:value]

  field :value, 1, type: :uint32
end

defmodule Google.Protobuf.BoolValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: boolean
        }

  defstruct [:value]

  field :value, 1, type: :bool
end

defmodule Google.Protobuf.StringValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: String.t()
        }

  defstruct [:value]

  field :value, 1, type: :string
end

defmodule Google.Protobuf.BytesValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: binary
        }

  defstruct [:value]

  field :value, 1, type: :bytes
end

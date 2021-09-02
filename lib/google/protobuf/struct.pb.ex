defmodule Google.Protobuf.NullValue do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3
  @type t :: integer | :NULL_VALUE

  field :NULL_VALUE, 0
end

defmodule Google.Protobuf.Struct.FieldsEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3

  @type t :: %__MODULE__{
          key: String.t(),
          value: Google.Protobuf.Value.t() | nil
        }

  defstruct [:key, :value]

  field :key, 1, type: :string
  field :value, 2, type: Google.Protobuf.Value
end

defmodule Google.Protobuf.Struct do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          fields: %{String.t() => Google.Protobuf.Value.t() | nil}
        }

  defstruct [:fields]

  field :fields, 1, repeated: true, type: Google.Protobuf.Struct.FieldsEntry, map: true
end

defmodule Google.Protobuf.Value do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          kind: {atom, any}
        }

  defstruct [:kind]

  oneof :kind, 0
  field :null_value, 1, type: Google.Protobuf.NullValue, enum: true, oneof: 0
  field :number_value, 2, type: :double, oneof: 0
  field :string_value, 3, type: :string, oneof: 0
  field :bool_value, 4, type: :bool, oneof: 0
  field :struct_value, 5, type: Google.Protobuf.Struct, oneof: 0
  field :list_value, 6, type: Google.Protobuf.ListValue, oneof: 0
end

defmodule Google.Protobuf.ListValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          values: [Google.Protobuf.Value.t()]
        }

  defstruct [:values]

  field :values, 1, repeated: true, type: Google.Protobuf.Value
end

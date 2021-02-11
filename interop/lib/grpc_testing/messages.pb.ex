defmodule Grpc.Testing.PayloadType do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :COMPRESSABLE

  field :COMPRESSABLE, 0
end

defmodule Grpc.Testing.BoolValue do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: boolean
        }
  defstruct [:value]

  field :value, 1, type: :bool
end

defmodule Grpc.Testing.Payload do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          type: Grpc.Testing.PayloadType.t(),
          body: binary
        }
  defstruct [:type, :body]

  field :type, 1, type: Grpc.Testing.PayloadType, enum: true
  field :body, 2, type: :bytes
end

defmodule Grpc.Testing.EchoStatus do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: integer,
          message: String.t()
        }
  defstruct [:code, :message]

  field :code, 1, type: :int32
  field :message, 2, type: :string
end

defmodule Grpc.Testing.SimpleRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response_type: Grpc.Testing.PayloadType.t(),
          response_size: integer,
          payload: Grpc.Testing.Payload.t() | nil,
          fill_username: boolean,
          fill_oauth_scope: boolean,
          response_compressed: Grpc.Testing.BoolValue.t() | nil,
          response_status: Grpc.Testing.EchoStatus.t() | nil,
          expect_compressed: Grpc.Testing.BoolValue.t() | nil,
          fill_server_id: boolean
        }
  defstruct [
    :response_type,
    :response_size,
    :payload,
    :fill_username,
    :fill_oauth_scope,
    :response_compressed,
    :response_status,
    :expect_compressed,
    :fill_server_id
  ]

  field :response_type, 1, type: Grpc.Testing.PayloadType, enum: true
  field :response_size, 2, type: :int32
  field :payload, 3, type: Grpc.Testing.Payload
  field :fill_username, 4, type: :bool
  field :fill_oauth_scope, 5, type: :bool
  field :response_compressed, 6, type: Grpc.Testing.BoolValue
  field :response_status, 7, type: Grpc.Testing.EchoStatus
  field :expect_compressed, 8, type: Grpc.Testing.BoolValue
  field :fill_server_id, 9, type: :bool
end

defmodule Grpc.Testing.SimpleResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          payload: Grpc.Testing.Payload.t() | nil,
          username: String.t(),
          oauth_scope: String.t(),
          server_id: String.t()
        }
  defstruct [:payload, :username, :oauth_scope, :server_id]

  field :payload, 1, type: Grpc.Testing.Payload
  field :username, 2, type: :string
  field :oauth_scope, 3, type: :string
  field :server_id, 4, type: :string
end

defmodule Grpc.Testing.StreamingInputCallRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          payload: Grpc.Testing.Payload.t() | nil,
          expect_compressed: Grpc.Testing.BoolValue.t() | nil
        }
  defstruct [:payload, :expect_compressed]

  field :payload, 1, type: Grpc.Testing.Payload
  field :expect_compressed, 2, type: Grpc.Testing.BoolValue
end

defmodule Grpc.Testing.StreamingInputCallResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          aggregated_payload_size: integer
        }
  defstruct [:aggregated_payload_size]

  field :aggregated_payload_size, 1, type: :int32
end

defmodule Grpc.Testing.ResponseParameters do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          size: integer,
          interval_us: integer,
          compressed: Grpc.Testing.BoolValue.t() | nil
        }
  defstruct [:size, :interval_us, :compressed]

  field :size, 1, type: :int32
  field :interval_us, 2, type: :int32
  field :compressed, 3, type: Grpc.Testing.BoolValue
end

defmodule Grpc.Testing.StreamingOutputCallRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response_type: Grpc.Testing.PayloadType.t(),
          response_parameters: [Grpc.Testing.ResponseParameters.t()],
          payload: Grpc.Testing.Payload.t() | nil,
          response_status: Grpc.Testing.EchoStatus.t() | nil
        }
  defstruct [:response_type, :response_parameters, :payload, :response_status]

  field :response_type, 1, type: Grpc.Testing.PayloadType, enum: true
  field :response_parameters, 2, repeated: true, type: Grpc.Testing.ResponseParameters
  field :payload, 3, type: Grpc.Testing.Payload
  field :response_status, 7, type: Grpc.Testing.EchoStatus
end

defmodule Grpc.Testing.StreamingOutputCallResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          payload: Grpc.Testing.Payload.t() | nil
        }
  defstruct [:payload]

  field :payload, 1, type: Grpc.Testing.Payload
end

defmodule Grpc.Testing.ReconnectParams do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          max_reconnect_backoff_ms: integer
        }
  defstruct [:max_reconnect_backoff_ms]

  field :max_reconnect_backoff_ms, 1, type: :int32
end

defmodule Grpc.Testing.ReconnectInfo do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          passed: boolean,
          backoff_ms: [integer]
        }
  defstruct [:passed, :backoff_ms]

  field :passed, 1, type: :bool
  field :backoff_ms, 2, repeated: true, type: :int32
end

defmodule Grpc.Testing.PayloadType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :COMPRESSABLE, 0
end

defmodule Grpc.Testing.BoolValue do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :value, 1, type: :bool
end

defmodule Grpc.Testing.Payload do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :type, 1, type: Grpc.Testing.PayloadType, enum: true
  field :body, 2, type: :bytes
end

defmodule Grpc.Testing.EchoStatus do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :code, 1, type: :int32
  field :message, 2, type: :string
end

defmodule Grpc.Testing.SimpleRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :response_type, 1, type: Grpc.Testing.PayloadType, json_name: "responseType", enum: true
  field :response_size, 2, type: :int32, json_name: "responseSize"
  field :payload, 3, type: Grpc.Testing.Payload
  field :fill_username, 4, type: :bool, json_name: "fillUsername"
  field :fill_oauth_scope, 5, type: :bool, json_name: "fillOauthScope"
  field :response_compressed, 6, type: Grpc.Testing.BoolValue, json_name: "responseCompressed"
  field :response_status, 7, type: Grpc.Testing.EchoStatus, json_name: "responseStatus"
  field :expect_compressed, 8, type: Grpc.Testing.BoolValue, json_name: "expectCompressed"
end

defmodule Grpc.Testing.SimpleResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
  field :username, 2, type: :string
  field :oauth_scope, 3, type: :string, json_name: "oauthScope"
end

defmodule Grpc.Testing.StreamingInputCallRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
  field :expect_compressed, 2, type: Grpc.Testing.BoolValue, json_name: "expectCompressed"
end

defmodule Grpc.Testing.StreamingInputCallResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :aggregated_payload_size, 1, type: :int32, json_name: "aggregatedPayloadSize"
end

defmodule Grpc.Testing.ResponseParameters do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :size, 1, type: :int32
  field :interval_us, 2, type: :int32, json_name: "intervalUs"
  field :compressed, 3, type: Grpc.Testing.BoolValue
end

defmodule Grpc.Testing.StreamingOutputCallRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :response_type, 1, type: Grpc.Testing.PayloadType, json_name: "responseType", enum: true

  field :response_parameters, 2,
    repeated: true,
    type: Grpc.Testing.ResponseParameters,
    json_name: "responseParameters"

  field :payload, 3, type: Grpc.Testing.Payload
  field :response_status, 7, type: Grpc.Testing.EchoStatus, json_name: "responseStatus"
end

defmodule Grpc.Testing.StreamingOutputCallResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :payload, 1, type: Grpc.Testing.Payload
end

defmodule Grpc.Testing.ReconnectParams do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :max_reconnect_backoff_ms, 1, type: :int32, json_name: "maxReconnectBackoffMs"
end

defmodule Grpc.Testing.ReconnectInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :passed, 1, type: :bool
  field :backoff_ms, 2, repeated: true, type: :int32, json_name: "backoffMs"
end
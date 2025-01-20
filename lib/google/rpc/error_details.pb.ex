defmodule Google.Rpc.ErrorInfo.MetadataEntry do
  @moduledoc false
  use Protobuf, map: true, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.ErrorInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :reason, 1, type: :string
  field :domain, 2, type: :string
  field :metadata, 3, repeated: true, type: Google.Rpc.ErrorInfo.MetadataEntry, map: true
end

defmodule Google.Rpc.RetryInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :retry_delay, 1, type: Google.Protobuf.Duration, json_name: "retryDelay"
end

defmodule Google.Rpc.DebugInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :stack_entries, 1, repeated: true, type: :string, json_name: "stackEntries"
  field :detail, 2, type: :string
end

defmodule Google.Rpc.QuotaFailure.Violation do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :subject, 1, type: :string
  field :description, 2, type: :string
end

defmodule Google.Rpc.QuotaFailure do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :violations, 1, repeated: true, type: Google.Rpc.QuotaFailure.Violation
end

defmodule Google.Rpc.PreconditionFailure.Violation do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :type, 1, type: :string
  field :subject, 2, type: :string
  field :description, 3, type: :string
end

defmodule Google.Rpc.PreconditionFailure do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :violations, 1, repeated: true, type: Google.Rpc.PreconditionFailure.Violation
end

defmodule Google.Rpc.BadRequest.FieldViolation do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :field, 1, type: :string
  field :description, 2, type: :string
end

defmodule Google.Rpc.BadRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :field_violations, 1,
    repeated: true,
    type: Google.Rpc.BadRequest.FieldViolation,
    json_name: "fieldViolations"
end

defmodule Google.Rpc.RequestInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :request_id, 1, type: :string, json_name: "requestId"
  field :serving_data, 2, type: :string, json_name: "servingData"
end

defmodule Google.Rpc.ResourceInfo do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :resource_type, 1, type: :string, json_name: "resourceType"
  field :resource_name, 2, type: :string, json_name: "resourceName"
  field :owner, 3, type: :string
  field :description, 4, type: :string
end

defmodule Google.Rpc.Help.Link do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :description, 1, type: :string
  field :url, 2, type: :string
end

defmodule Google.Rpc.Help do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :links, 1, repeated: true, type: Google.Rpc.Help.Link
end

defmodule Google.Rpc.LocalizedMessage do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.0", syntax: :proto3

  field :locale, 1, type: :string
  field :message, 2, type: :string
end

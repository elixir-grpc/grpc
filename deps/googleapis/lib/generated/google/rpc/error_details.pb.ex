defmodule Google.Rpc.ErrorInfo.MetadataEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.ErrorInfo do
  @moduledoc """
  Describes the cause of the error with structured details.

  Example of an error when contacting the "pubsub.googleapis.com" API when it
  is not enabled:

      { "reason": "API_DISABLED"
        "domain": "googleapis.com"
        "metadata": {
          "resource": "projects/123",
          "service": "pubsub.googleapis.com"
        }
      }

  This response indicates that the pubsub.googleapis.com API is not enabled.

  Example of an error that is returned when attempting to create a Spanner
  instance in a region that is out of stock:

      { "reason": "STOCKOUT"
        "domain": "spanner.googleapis.com",
        "metadata": {
          "availableRegions": "us-central1,us-east2"
        }
      }
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :reason, 1, type: :string
  field :domain, 2, type: :string
  field :metadata, 3, repeated: true, type: Google.Rpc.ErrorInfo.MetadataEntry, map: true
end

defmodule Google.Rpc.RetryInfo do
  @moduledoc """
  Describes when the clients can retry a failed request. Clients could ignore
  the recommendation here or retry when this information is missing from error
  responses.

  It's always recommended that clients should use exponential backoff when
  retrying.

  Clients should wait until `retry_delay` amount of time has passed since
  receiving the error response before retrying.  If retrying requests also
  fail, clients should use an exponential backoff scheme to gradually increase
  the delay between retries based on `retry_delay`, until either a maximum
  number of retries have been reached or a maximum retry delay cap has been
  reached.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :retry_delay, 1, type: Google.Protobuf.Duration, json_name: "retryDelay"
end

defmodule Google.Rpc.DebugInfo do
  @moduledoc """
  Describes additional debugging info.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :stack_entries, 1, repeated: true, type: :string, json_name: "stackEntries"
  field :detail, 2, type: :string
end

defmodule Google.Rpc.QuotaFailure.Violation.QuotaDimensionsEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Rpc.QuotaFailure.Violation do
  @moduledoc """
  A message type used to describe a single quota violation.  For example, a
  daily quota or a custom quota that was exceeded.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :subject, 1, type: :string
  field :description, 2, type: :string
  field :api_service, 3, type: :string, json_name: "apiService"
  field :quota_metric, 4, type: :string, json_name: "quotaMetric"
  field :quota_id, 5, type: :string, json_name: "quotaId"

  field :quota_dimensions, 6,
    repeated: true,
    type: Google.Rpc.QuotaFailure.Violation.QuotaDimensionsEntry,
    json_name: "quotaDimensions",
    map: true

  field :quota_value, 7, type: :int64, json_name: "quotaValue"
  field :future_quota_value, 8, proto3_optional: true, type: :int64, json_name: "futureQuotaValue"
end

defmodule Google.Rpc.QuotaFailure do
  @moduledoc """
  Describes how a quota check failed.

  For example if a daily limit was exceeded for the calling project,
  a service could respond with a QuotaFailure detail containing the project
  id and the description of the quota limit that was exceeded.  If the
  calling project hasn't enabled the service in the developer console, then
  a service could respond with the project id and set `service_disabled`
  to true.

  Also see RetryInfo and Help types for other details about handling a
  quota failure.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :violations, 1, repeated: true, type: Google.Rpc.QuotaFailure.Violation
end

defmodule Google.Rpc.PreconditionFailure.Violation do
  @moduledoc """
  A message type used to describe a single precondition failure.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: :string
  field :subject, 2, type: :string
  field :description, 3, type: :string
end

defmodule Google.Rpc.PreconditionFailure do
  @moduledoc """
  Describes what preconditions have failed.

  For example, if an RPC failed because it required the Terms of Service to be
  acknowledged, it could list the terms of service violation in the
  PreconditionFailure message.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :violations, 1, repeated: true, type: Google.Rpc.PreconditionFailure.Violation
end

defmodule Google.Rpc.BadRequest.FieldViolation do
  @moduledoc """
  A message type used to describe a single bad request field.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :field, 1, type: :string
  field :description, 2, type: :string
  field :reason, 3, type: :string
  field :localized_message, 4, type: Google.Rpc.LocalizedMessage, json_name: "localizedMessage"
end

defmodule Google.Rpc.BadRequest do
  @moduledoc """
  Describes violations in a client request. This error type focuses on the
  syntactic aspects of the request.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :field_violations, 1,
    repeated: true,
    type: Google.Rpc.BadRequest.FieldViolation,
    json_name: "fieldViolations"
end

defmodule Google.Rpc.RequestInfo do
  @moduledoc """
  Contains metadata about the request that clients can attach when filing a bug
  or providing other forms of feedback.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :request_id, 1, type: :string, json_name: "requestId"
  field :serving_data, 2, type: :string, json_name: "servingData"
end

defmodule Google.Rpc.ResourceInfo do
  @moduledoc """
  Describes the resource that is being accessed.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :resource_type, 1, type: :string, json_name: "resourceType"
  field :resource_name, 2, type: :string, json_name: "resourceName"
  field :owner, 3, type: :string
  field :description, 4, type: :string
end

defmodule Google.Rpc.Help.Link do
  @moduledoc """
  Describes a URL link.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :description, 1, type: :string
  field :url, 2, type: :string
end

defmodule Google.Rpc.Help do
  @moduledoc """
  Provides links to documentation or for performing an out of band action.

  For example, if a quota check failed with an error indicating the calling
  project hasn't enabled the accessed service, this can contain a URL pointing
  directly to the right place in the developer console to flip the bit.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :links, 1, repeated: true, type: Google.Rpc.Help.Link
end

defmodule Google.Rpc.LocalizedMessage do
  @moduledoc """
  Provides a localized error message that is safe to return to the user
  which can be attached to an RPC error.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :locale, 1, type: :string
  field :message, 2, type: :string
end

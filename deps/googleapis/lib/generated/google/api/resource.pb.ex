defmodule Google.Api.ResourceDescriptor.History do
  @moduledoc """
  A description of the historical or future-looking state of the
  resource pattern.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :HISTORY_UNSPECIFIED, 0
  field :ORIGINALLY_SINGLE_PATTERN, 1
  field :FUTURE_MULTI_PATTERN, 2
end

defmodule Google.Api.ResourceDescriptor.Style do
  @moduledoc """
  A flag representing a specific style that a resource claims to conform to.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :STYLE_UNSPECIFIED, 0
  field :DECLARATIVE_FRIENDLY, 1
end

defmodule Google.Api.ResourceDescriptor do
  @moduledoc """
  A simple descriptor of a resource type.

  ResourceDescriptor annotates a resource message (either by means of a
  protobuf annotation or use in the service config), and associates the
  resource's schema, the resource type, and the pattern of the resource name.

  Example:

      message Topic {
        // Indicates this message defines a resource schema.
        // Declares the resource type in the format of {service}/{kind}.
        // For Kubernetes resources, the format is {api group}/{kind}.
        option (google.api.resource) = {
          type: "pubsub.googleapis.com/Topic"
          pattern: "projects/{project}/topics/{topic}"
        };
      }

  The ResourceDescriptor Yaml config will look like:

      resources:
      - type: "pubsub.googleapis.com/Topic"
        pattern: "projects/{project}/topics/{topic}"

  Sometimes, resources have multiple patterns, typically because they can
  live under multiple parents.

  Example:

      message LogEntry {
        option (google.api.resource) = {
          type: "logging.googleapis.com/LogEntry"
          pattern: "projects/{project}/logs/{log}"
          pattern: "folders/{folder}/logs/{log}"
          pattern: "organizations/{organization}/logs/{log}"
          pattern: "billingAccounts/{billing_account}/logs/{log}"
        };
      }

  The ResourceDescriptor Yaml config will look like:

      resources:
      - type: 'logging.googleapis.com/LogEntry'
        pattern: "projects/{project}/logs/{log}"
        pattern: "folders/{folder}/logs/{log}"
        pattern: "organizations/{organization}/logs/{log}"
        pattern: "billingAccounts/{billing_account}/logs/{log}"
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: :string
  field :pattern, 2, repeated: true, type: :string
  field :name_field, 3, type: :string, json_name: "nameField"
  field :history, 4, type: Google.Api.ResourceDescriptor.History, enum: true
  field :plural, 5, type: :string
  field :singular, 6, type: :string
  field :style, 10, repeated: true, type: Google.Api.ResourceDescriptor.Style, enum: true
end

defmodule Google.Api.ResourceReference do
  @moduledoc """
  Defines a proto annotation that describes a string field that refers to
  an API resource.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :type, 1, type: :string
  field :child_type, 2, type: :string, json_name: "childType"
end

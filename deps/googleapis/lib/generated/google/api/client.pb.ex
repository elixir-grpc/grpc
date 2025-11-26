defmodule Google.Api.ClientLibraryOrganization do
  @moduledoc """
  The organization for which the client libraries are being published.
  Affects the url where generated docs are published, etc.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :CLIENT_LIBRARY_ORGANIZATION_UNSPECIFIED, 0
  field :CLOUD, 1
  field :ADS, 2
  field :PHOTOS, 3
  field :STREET_VIEW, 4
  field :SHOPPING, 5
  field :GEO, 6
  field :GENERATIVE_AI, 7
end

defmodule Google.Api.ClientLibraryDestination do
  @moduledoc """
  To where should client libraries be published?
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :CLIENT_LIBRARY_DESTINATION_UNSPECIFIED, 0
  field :GITHUB, 10
  field :PACKAGE_MANAGER, 20
end

defmodule Google.Api.CommonLanguageSettings do
  @moduledoc """
  Required information for every language.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :reference_docs_uri, 1, type: :string, json_name: "referenceDocsUri", deprecated: true
  field :destinations, 2, repeated: true, type: Google.Api.ClientLibraryDestination, enum: true

  field :selective_gapic_generation, 3,
    type: Google.Api.SelectiveGapicGeneration,
    json_name: "selectiveGapicGeneration"
end

defmodule Google.Api.ClientLibrarySettings do
  @moduledoc """
  Details about how and where to publish client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :version, 1, type: :string
  field :launch_stage, 2, type: Google.Api.LaunchStage, json_name: "launchStage", enum: true
  field :rest_numeric_enums, 3, type: :bool, json_name: "restNumericEnums"
  field :java_settings, 21, type: Google.Api.JavaSettings, json_name: "javaSettings"
  field :cpp_settings, 22, type: Google.Api.CppSettings, json_name: "cppSettings"
  field :php_settings, 23, type: Google.Api.PhpSettings, json_name: "phpSettings"
  field :python_settings, 24, type: Google.Api.PythonSettings, json_name: "pythonSettings"
  field :node_settings, 25, type: Google.Api.NodeSettings, json_name: "nodeSettings"
  field :dotnet_settings, 26, type: Google.Api.DotnetSettings, json_name: "dotnetSettings"
  field :ruby_settings, 27, type: Google.Api.RubySettings, json_name: "rubySettings"
  field :go_settings, 28, type: Google.Api.GoSettings, json_name: "goSettings"
end

defmodule Google.Api.Publishing do
  @moduledoc """
  This message configures the settings for publishing [Google Cloud Client
  libraries](https://cloud.google.com/apis/docs/cloud-client-libraries)
  generated from the service config.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :method_settings, 2,
    repeated: true,
    type: Google.Api.MethodSettings,
    json_name: "methodSettings"

  field :new_issue_uri, 101, type: :string, json_name: "newIssueUri"
  field :documentation_uri, 102, type: :string, json_name: "documentationUri"
  field :api_short_name, 103, type: :string, json_name: "apiShortName"
  field :github_label, 104, type: :string, json_name: "githubLabel"

  field :codeowner_github_teams, 105,
    repeated: true,
    type: :string,
    json_name: "codeownerGithubTeams"

  field :doc_tag_prefix, 106, type: :string, json_name: "docTagPrefix"
  field :organization, 107, type: Google.Api.ClientLibraryOrganization, enum: true

  field :library_settings, 109,
    repeated: true,
    type: Google.Api.ClientLibrarySettings,
    json_name: "librarySettings"

  field :proto_reference_documentation_uri, 110,
    type: :string,
    json_name: "protoReferenceDocumentationUri"

  field :rest_reference_documentation_uri, 111,
    type: :string,
    json_name: "restReferenceDocumentationUri"
end

defmodule Google.Api.JavaSettings.ServiceClassNamesEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Api.JavaSettings do
  @moduledoc """
  Settings for Java client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :library_package, 1, type: :string, json_name: "libraryPackage"

  field :service_class_names, 2,
    repeated: true,
    type: Google.Api.JavaSettings.ServiceClassNamesEntry,
    json_name: "serviceClassNames",
    map: true

  field :common, 3, type: Google.Api.CommonLanguageSettings
end

defmodule Google.Api.CppSettings do
  @moduledoc """
  Settings for C++ client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings
end

defmodule Google.Api.PhpSettings do
  @moduledoc """
  Settings for Php client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings
end

defmodule Google.Api.PythonSettings.ExperimentalFeatures do
  @moduledoc """
  Experimental features to be included during client library generation.
  These fields will be deprecated once the feature graduates and is enabled
  by default.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :rest_async_io_enabled, 1, type: :bool, json_name: "restAsyncIoEnabled"

  field :protobuf_pythonic_types_enabled, 2,
    type: :bool,
    json_name: "protobufPythonicTypesEnabled"

  field :unversioned_package_disabled, 3, type: :bool, json_name: "unversionedPackageDisabled"
end

defmodule Google.Api.PythonSettings do
  @moduledoc """
  Settings for Python client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings

  field :experimental_features, 2,
    type: Google.Api.PythonSettings.ExperimentalFeatures,
    json_name: "experimentalFeatures"
end

defmodule Google.Api.NodeSettings do
  @moduledoc """
  Settings for Node client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings
end

defmodule Google.Api.DotnetSettings.RenamedServicesEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Api.DotnetSettings.RenamedResourcesEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Api.DotnetSettings do
  @moduledoc """
  Settings for Dotnet client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings

  field :renamed_services, 2,
    repeated: true,
    type: Google.Api.DotnetSettings.RenamedServicesEntry,
    json_name: "renamedServices",
    map: true

  field :renamed_resources, 3,
    repeated: true,
    type: Google.Api.DotnetSettings.RenamedResourcesEntry,
    json_name: "renamedResources",
    map: true

  field :ignored_resources, 4, repeated: true, type: :string, json_name: "ignoredResources"

  field :forced_namespace_aliases, 5,
    repeated: true,
    type: :string,
    json_name: "forcedNamespaceAliases"

  field :handwritten_signatures, 6,
    repeated: true,
    type: :string,
    json_name: "handwrittenSignatures"
end

defmodule Google.Api.RubySettings do
  @moduledoc """
  Settings for Ruby client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings
end

defmodule Google.Api.GoSettings.RenamedServicesEntry do
  use Protobuf, map: true, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Google.Api.GoSettings do
  @moduledoc """
  Settings for Go client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :common, 1, type: Google.Api.CommonLanguageSettings

  field :renamed_services, 2,
    repeated: true,
    type: Google.Api.GoSettings.RenamedServicesEntry,
    json_name: "renamedServices",
    map: true
end

defmodule Google.Api.MethodSettings.LongRunning do
  @moduledoc """
  Describes settings to use when generating API methods that use the
  long-running operation pattern.
  All default values below are from those used in the client library
  generators (e.g.
  [Java](https://github.com/googleapis/gapic-generator-java/blob/04c2faa191a9b5a10b92392fe8482279c4404803/src/main/java/com/google/api/generator/gapic/composer/common/RetrySettingsComposer.java)).
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :initial_poll_delay, 1, type: Google.Protobuf.Duration, json_name: "initialPollDelay"
  field :poll_delay_multiplier, 2, type: :float, json_name: "pollDelayMultiplier"
  field :max_poll_delay, 3, type: Google.Protobuf.Duration, json_name: "maxPollDelay"
  field :total_poll_timeout, 4, type: Google.Protobuf.Duration, json_name: "totalPollTimeout"
end

defmodule Google.Api.MethodSettings do
  @moduledoc """
  Describes the generator configuration for a method.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :selector, 1, type: :string
  field :long_running, 2, type: Google.Api.MethodSettings.LongRunning, json_name: "longRunning"
  field :auto_populated_fields, 3, repeated: true, type: :string, json_name: "autoPopulatedFields"
end

defmodule Google.Api.SelectiveGapicGeneration do
  @moduledoc """
  This message is used to configure the generation of a subset of the RPCs in
  a service for client libraries.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :methods, 1, repeated: true, type: :string
  field :generate_omitted_as_internal, 2, type: :bool, json_name: "generateOmittedAsInternal"
end

defmodule Google.Protobuf.Edition do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :EDITION_UNKNOWN, 0
  field :EDITION_LEGACY, 900
  field :EDITION_PROTO2, 998
  field :EDITION_PROTO3, 999
  field :EDITION_2023, 1000
  field :EDITION_2024, 1001
  field :EDITION_1_TEST_ONLY, 1
  field :EDITION_2_TEST_ONLY, 2
  field :EDITION_99997_TEST_ONLY, 99997
  field :EDITION_99998_TEST_ONLY, 99998
  field :EDITION_99999_TEST_ONLY, 99999
  field :EDITION_MAX, 2_147_483_647
end

defmodule Google.Protobuf.ExtensionRangeOptions.VerificationState do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :DECLARATION, 0
  field :UNVERIFIED, 1
end

defmodule Google.Protobuf.FieldDescriptorProto.Type do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :TYPE_DOUBLE, 1
  field :TYPE_FLOAT, 2
  field :TYPE_INT64, 3
  field :TYPE_UINT64, 4
  field :TYPE_INT32, 5
  field :TYPE_FIXED64, 6
  field :TYPE_FIXED32, 7
  field :TYPE_BOOL, 8
  field :TYPE_STRING, 9
  field :TYPE_GROUP, 10
  field :TYPE_MESSAGE, 11
  field :TYPE_BYTES, 12
  field :TYPE_UINT32, 13
  field :TYPE_ENUM, 14
  field :TYPE_SFIXED32, 15
  field :TYPE_SFIXED64, 16
  field :TYPE_SINT32, 17
  field :TYPE_SINT64, 18
end

defmodule Google.Protobuf.FieldDescriptorProto.Label do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :LABEL_OPTIONAL, 1
  field :LABEL_REPEATED, 3
  field :LABEL_REQUIRED, 2
end

defmodule Google.Protobuf.FileOptions.OptimizeMode do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :SPEED, 1
  field :CODE_SIZE, 2
  field :LITE_RUNTIME, 3
end

defmodule Google.Protobuf.FieldOptions.CType do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :STRING, 0
  field :CORD, 1
  field :STRING_PIECE, 2
end

defmodule Google.Protobuf.FieldOptions.JSType do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :JS_NORMAL, 0
  field :JS_STRING, 1
  field :JS_NUMBER, 2
end

defmodule Google.Protobuf.FieldOptions.OptionRetention do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :RETENTION_UNKNOWN, 0
  field :RETENTION_RUNTIME, 1
  field :RETENTION_SOURCE, 2
end

defmodule Google.Protobuf.FieldOptions.OptionTargetType do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :TARGET_TYPE_UNKNOWN, 0
  field :TARGET_TYPE_FILE, 1
  field :TARGET_TYPE_EXTENSION_RANGE, 2
  field :TARGET_TYPE_MESSAGE, 3
  field :TARGET_TYPE_FIELD, 4
  field :TARGET_TYPE_ONEOF, 5
  field :TARGET_TYPE_ENUM, 6
  field :TARGET_TYPE_ENUM_ENTRY, 7
  field :TARGET_TYPE_SERVICE, 8
  field :TARGET_TYPE_METHOD, 9
end

defmodule Google.Protobuf.MethodOptions.IdempotencyLevel do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :IDEMPOTENCY_UNKNOWN, 0
  field :NO_SIDE_EFFECTS, 1
  field :IDEMPOTENT, 2
end

defmodule Google.Protobuf.FeatureSet.FieldPresence do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :FIELD_PRESENCE_UNKNOWN, 0
  field :EXPLICIT, 1
  field :IMPLICIT, 2
  field :LEGACY_REQUIRED, 3
end

defmodule Google.Protobuf.FeatureSet.EnumType do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :ENUM_TYPE_UNKNOWN, 0
  field :OPEN, 1
  field :CLOSED, 2
end

defmodule Google.Protobuf.FeatureSet.RepeatedFieldEncoding do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :REPEATED_FIELD_ENCODING_UNKNOWN, 0
  field :PACKED, 1
  field :EXPANDED, 2
end

defmodule Google.Protobuf.FeatureSet.Utf8Validation do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :UTF8_VALIDATION_UNKNOWN, 0
  field :VERIFY, 2
  field :NONE, 3
end

defmodule Google.Protobuf.FeatureSet.MessageEncoding do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :MESSAGE_ENCODING_UNKNOWN, 0
  field :LENGTH_PREFIXED, 1
  field :DELIMITED, 2
end

defmodule Google.Protobuf.FeatureSet.JsonFormat do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :JSON_FORMAT_UNKNOWN, 0
  field :ALLOW, 1
  field :LEGACY_BEST_EFFORT, 2
end

defmodule Google.Protobuf.GeneratedCodeInfo.Annotation.Semantic do
  @moduledoc false

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :NONE, 0
  field :SET, 1
  field :ALIAS, 2
end

defmodule Google.Protobuf.FileDescriptorSet do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :file, 1, repeated: true, type: Google.Protobuf.FileDescriptorProto

  extensions [{536_000_000, 536_000_001}]
end

defmodule Google.Protobuf.FileDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :package, 2, optional: true, type: :string
  field :dependency, 3, repeated: true, type: :string
  field :public_dependency, 10, repeated: true, type: :int32, json_name: "publicDependency"
  field :weak_dependency, 11, repeated: true, type: :int32, json_name: "weakDependency"

  field :message_type, 4,
    repeated: true,
    type: Google.Protobuf.DescriptorProto,
    json_name: "messageType"

  field :enum_type, 5,
    repeated: true,
    type: Google.Protobuf.EnumDescriptorProto,
    json_name: "enumType"

  field :service, 6, repeated: true, type: Google.Protobuf.ServiceDescriptorProto
  field :extension, 7, repeated: true, type: Google.Protobuf.FieldDescriptorProto
  field :options, 8, optional: true, type: Google.Protobuf.FileOptions

  field :source_code_info, 9,
    optional: true,
    type: Google.Protobuf.SourceCodeInfo,
    json_name: "sourceCodeInfo"

  field :syntax, 12, optional: true, type: :string
  field :edition, 14, optional: true, type: Google.Protobuf.Edition, enum: true
end

defmodule Google.Protobuf.DescriptorProto.ExtensionRange do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :start, 1, optional: true, type: :int32
  field :end, 2, optional: true, type: :int32
  field :options, 3, optional: true, type: Google.Protobuf.ExtensionRangeOptions
end

defmodule Google.Protobuf.DescriptorProto.ReservedRange do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :start, 1, optional: true, type: :int32
  field :end, 2, optional: true, type: :int32
end

defmodule Google.Protobuf.DescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :field, 2, repeated: true, type: Google.Protobuf.FieldDescriptorProto
  field :extension, 6, repeated: true, type: Google.Protobuf.FieldDescriptorProto

  field :nested_type, 3,
    repeated: true,
    type: Google.Protobuf.DescriptorProto,
    json_name: "nestedType"

  field :enum_type, 4,
    repeated: true,
    type: Google.Protobuf.EnumDescriptorProto,
    json_name: "enumType"

  field :extension_range, 5,
    repeated: true,
    type: Google.Protobuf.DescriptorProto.ExtensionRange,
    json_name: "extensionRange"

  field :oneof_decl, 8,
    repeated: true,
    type: Google.Protobuf.OneofDescriptorProto,
    json_name: "oneofDecl"

  field :options, 7, optional: true, type: Google.Protobuf.MessageOptions

  field :reserved_range, 9,
    repeated: true,
    type: Google.Protobuf.DescriptorProto.ReservedRange,
    json_name: "reservedRange"

  field :reserved_name, 10, repeated: true, type: :string, json_name: "reservedName"
end

defmodule Google.Protobuf.ExtensionRangeOptions.Declaration do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :number, 1, optional: true, type: :int32
  field :full_name, 2, optional: true, type: :string, json_name: "fullName"
  field :type, 3, optional: true, type: :string
  field :reserved, 5, optional: true, type: :bool
  field :repeated, 6, optional: true, type: :bool
end

defmodule Google.Protobuf.ExtensionRangeOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  field :declaration, 2,
    repeated: true,
    type: Google.Protobuf.ExtensionRangeOptions.Declaration,
    deprecated: false

  field :features, 50, optional: true, type: Google.Protobuf.FeatureSet

  field :verification, 3,
    optional: true,
    type: Google.Protobuf.ExtensionRangeOptions.VerificationState,
    default: :UNVERIFIED,
    enum: true,
    deprecated: false

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.FieldDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :number, 3, optional: true, type: :int32
  field :label, 4, optional: true, type: Google.Protobuf.FieldDescriptorProto.Label, enum: true
  field :type, 5, optional: true, type: Google.Protobuf.FieldDescriptorProto.Type, enum: true
  field :type_name, 6, optional: true, type: :string, json_name: "typeName"
  field :extendee, 2, optional: true, type: :string
  field :default_value, 7, optional: true, type: :string, json_name: "defaultValue"
  field :oneof_index, 9, optional: true, type: :int32, json_name: "oneofIndex"
  field :json_name, 10, optional: true, type: :string, json_name: "jsonName"
  field :options, 8, optional: true, type: Google.Protobuf.FieldOptions
  field :proto3_optional, 17, optional: true, type: :bool, json_name: "proto3Optional"
end

defmodule Google.Protobuf.OneofDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :options, 2, optional: true, type: Google.Protobuf.OneofOptions
end

defmodule Google.Protobuf.EnumDescriptorProto.EnumReservedRange do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :start, 1, optional: true, type: :int32
  field :end, 2, optional: true, type: :int32
end

defmodule Google.Protobuf.EnumDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :value, 2, repeated: true, type: Google.Protobuf.EnumValueDescriptorProto
  field :options, 3, optional: true, type: Google.Protobuf.EnumOptions

  field :reserved_range, 4,
    repeated: true,
    type: Google.Protobuf.EnumDescriptorProto.EnumReservedRange,
    json_name: "reservedRange"

  field :reserved_name, 5, repeated: true, type: :string, json_name: "reservedName"
end

defmodule Google.Protobuf.EnumValueDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :number, 2, optional: true, type: :int32
  field :options, 3, optional: true, type: Google.Protobuf.EnumValueOptions
end

defmodule Google.Protobuf.ServiceDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :method, 2, repeated: true, type: Google.Protobuf.MethodDescriptorProto
  field :options, 3, optional: true, type: Google.Protobuf.ServiceOptions
end

defmodule Google.Protobuf.MethodDescriptorProto do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 1, optional: true, type: :string
  field :input_type, 2, optional: true, type: :string, json_name: "inputType"
  field :output_type, 3, optional: true, type: :string, json_name: "outputType"
  field :options, 4, optional: true, type: Google.Protobuf.MethodOptions

  field :client_streaming, 5,
    optional: true,
    type: :bool,
    json_name: "clientStreaming",
    default: false

  field :server_streaming, 6,
    optional: true,
    type: :bool,
    json_name: "serverStreaming",
    default: false
end

defmodule Google.Protobuf.FileOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :java_package, 1, optional: true, type: :string, json_name: "javaPackage"
  field :java_outer_classname, 8, optional: true, type: :string, json_name: "javaOuterClassname"

  field :java_multiple_files, 10,
    optional: true,
    type: :bool,
    json_name: "javaMultipleFiles",
    default: false

  field :java_generate_equals_and_hash, 20,
    optional: true,
    type: :bool,
    json_name: "javaGenerateEqualsAndHash",
    deprecated: true

  field :java_string_check_utf8, 27,
    optional: true,
    type: :bool,
    json_name: "javaStringCheckUtf8",
    default: false

  field :optimize_for, 9,
    optional: true,
    type: Google.Protobuf.FileOptions.OptimizeMode,
    json_name: "optimizeFor",
    default: :SPEED,
    enum: true

  field :go_package, 11, optional: true, type: :string, json_name: "goPackage"

  field :cc_generic_services, 16,
    optional: true,
    type: :bool,
    json_name: "ccGenericServices",
    default: false

  field :java_generic_services, 17,
    optional: true,
    type: :bool,
    json_name: "javaGenericServices",
    default: false

  field :py_generic_services, 18,
    optional: true,
    type: :bool,
    json_name: "pyGenericServices",
    default: false

  field :deprecated, 23, optional: true, type: :bool, default: false

  field :cc_enable_arenas, 31,
    optional: true,
    type: :bool,
    json_name: "ccEnableArenas",
    default: true

  field :objc_class_prefix, 36, optional: true, type: :string, json_name: "objcClassPrefix"
  field :csharp_namespace, 37, optional: true, type: :string, json_name: "csharpNamespace"
  field :swift_prefix, 39, optional: true, type: :string, json_name: "swiftPrefix"
  field :php_class_prefix, 40, optional: true, type: :string, json_name: "phpClassPrefix"
  field :php_namespace, 41, optional: true, type: :string, json_name: "phpNamespace"

  field :php_metadata_namespace, 44,
    optional: true,
    type: :string,
    json_name: "phpMetadataNamespace"

  field :ruby_package, 45, optional: true, type: :string, json_name: "rubyPackage"
  field :features, 50, optional: true, type: Google.Protobuf.FeatureSet

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.MessageOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :message_set_wire_format, 1,
    optional: true,
    type: :bool,
    json_name: "messageSetWireFormat",
    default: false

  field :no_standard_descriptor_accessor, 2,
    optional: true,
    type: :bool,
    json_name: "noStandardDescriptorAccessor",
    default: false

  field :deprecated, 3, optional: true, type: :bool, default: false
  field :map_entry, 7, optional: true, type: :bool, json_name: "mapEntry"

  field :deprecated_legacy_json_field_conflicts, 11,
    optional: true,
    type: :bool,
    json_name: "deprecatedLegacyJsonFieldConflicts",
    deprecated: true

  field :features, 12, optional: true, type: Google.Protobuf.FeatureSet

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.FieldOptions.EditionDefault do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :edition, 3, optional: true, type: Google.Protobuf.Edition, enum: true
  field :value, 2, optional: true, type: :string
end

defmodule Google.Protobuf.FieldOptions.FeatureSupport do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :edition_introduced, 1,
    optional: true,
    type: Google.Protobuf.Edition,
    json_name: "editionIntroduced",
    enum: true

  field :edition_deprecated, 2,
    optional: true,
    type: Google.Protobuf.Edition,
    json_name: "editionDeprecated",
    enum: true

  field :deprecation_warning, 3, optional: true, type: :string, json_name: "deprecationWarning"

  field :edition_removed, 4,
    optional: true,
    type: Google.Protobuf.Edition,
    json_name: "editionRemoved",
    enum: true
end

defmodule Google.Protobuf.FieldOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :ctype, 1,
    optional: true,
    type: Google.Protobuf.FieldOptions.CType,
    default: :STRING,
    enum: true

  field :packed, 2, optional: true, type: :bool

  field :jstype, 6,
    optional: true,
    type: Google.Protobuf.FieldOptions.JSType,
    default: :JS_NORMAL,
    enum: true

  field :lazy, 5, optional: true, type: :bool, default: false

  field :unverified_lazy, 15,
    optional: true,
    type: :bool,
    json_name: "unverifiedLazy",
    default: false

  field :deprecated, 3, optional: true, type: :bool, default: false
  field :weak, 10, optional: true, type: :bool, default: false
  field :debug_redact, 16, optional: true, type: :bool, json_name: "debugRedact", default: false

  field :retention, 17,
    optional: true,
    type: Google.Protobuf.FieldOptions.OptionRetention,
    enum: true

  field :targets, 19,
    repeated: true,
    type: Google.Protobuf.FieldOptions.OptionTargetType,
    enum: true

  field :edition_defaults, 20,
    repeated: true,
    type: Google.Protobuf.FieldOptions.EditionDefault,
    json_name: "editionDefaults"

  field :features, 21, optional: true, type: Google.Protobuf.FeatureSet

  field :feature_support, 22,
    optional: true,
    type: Google.Protobuf.FieldOptions.FeatureSupport,
    json_name: "featureSupport"

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.OneofOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :features, 1, optional: true, type: Google.Protobuf.FeatureSet

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.EnumOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :allow_alias, 2, optional: true, type: :bool, json_name: "allowAlias"
  field :deprecated, 3, optional: true, type: :bool, default: false

  field :deprecated_legacy_json_field_conflicts, 6,
    optional: true,
    type: :bool,
    json_name: "deprecatedLegacyJsonFieldConflicts",
    deprecated: true

  field :features, 7, optional: true, type: Google.Protobuf.FeatureSet

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.EnumValueOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :deprecated, 1, optional: true, type: :bool, default: false
  field :features, 2, optional: true, type: Google.Protobuf.FeatureSet
  field :debug_redact, 3, optional: true, type: :bool, json_name: "debugRedact", default: false

  field :feature_support, 4,
    optional: true,
    type: Google.Protobuf.FieldOptions.FeatureSupport,
    json_name: "featureSupport"

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.ServiceOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :features, 34, optional: true, type: Google.Protobuf.FeatureSet
  field :deprecated, 33, optional: true, type: :bool, default: false

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.MethodOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :deprecated, 33, optional: true, type: :bool, default: false

  field :idempotency_level, 34,
    optional: true,
    type: Google.Protobuf.MethodOptions.IdempotencyLevel,
    json_name: "idempotencyLevel",
    default: :IDEMPOTENCY_UNKNOWN,
    enum: true

  field :features, 35, optional: true, type: Google.Protobuf.FeatureSet

  field :uninterpreted_option, 999,
    repeated: true,
    type: Google.Protobuf.UninterpretedOption,
    json_name: "uninterpretedOption"

  extensions [{1000, Protobuf.Extension.max()}]
end

defmodule Google.Protobuf.UninterpretedOption.NamePart do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name_part, 1, required: true, type: :string, json_name: "namePart"
  field :is_extension, 2, required: true, type: :bool, json_name: "isExtension"
end

defmodule Google.Protobuf.UninterpretedOption do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :name, 2, repeated: true, type: Google.Protobuf.UninterpretedOption.NamePart
  field :identifier_value, 3, optional: true, type: :string, json_name: "identifierValue"
  field :positive_int_value, 4, optional: true, type: :uint64, json_name: "positiveIntValue"
  field :negative_int_value, 5, optional: true, type: :int64, json_name: "negativeIntValue"
  field :double_value, 6, optional: true, type: :double, json_name: "doubleValue"
  field :string_value, 7, optional: true, type: :bytes, json_name: "stringValue"
  field :aggregate_value, 8, optional: true, type: :string, json_name: "aggregateValue"
end

defmodule Google.Protobuf.FeatureSet do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :field_presence, 1,
    optional: true,
    type: Google.Protobuf.FeatureSet.FieldPresence,
    json_name: "fieldPresence",
    enum: true,
    deprecated: false

  field :enum_type, 2,
    optional: true,
    type: Google.Protobuf.FeatureSet.EnumType,
    json_name: "enumType",
    enum: true,
    deprecated: false

  field :repeated_field_encoding, 3,
    optional: true,
    type: Google.Protobuf.FeatureSet.RepeatedFieldEncoding,
    json_name: "repeatedFieldEncoding",
    enum: true,
    deprecated: false

  field :utf8_validation, 4,
    optional: true,
    type: Google.Protobuf.FeatureSet.Utf8Validation,
    json_name: "utf8Validation",
    enum: true,
    deprecated: false

  field :message_encoding, 5,
    optional: true,
    type: Google.Protobuf.FeatureSet.MessageEncoding,
    json_name: "messageEncoding",
    enum: true,
    deprecated: false

  field :json_format, 6,
    optional: true,
    type: Google.Protobuf.FeatureSet.JsonFormat,
    json_name: "jsonFormat",
    enum: true,
    deprecated: false

  extensions [{1000, 9995}, {9995, 10000}, {10000, 10001}]
end

defmodule Google.Protobuf.FeatureSetDefaults.FeatureSetEditionDefault do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :edition, 3, optional: true, type: Google.Protobuf.Edition, enum: true

  field :overridable_features, 4,
    optional: true,
    type: Google.Protobuf.FeatureSet,
    json_name: "overridableFeatures"

  field :fixed_features, 5,
    optional: true,
    type: Google.Protobuf.FeatureSet,
    json_name: "fixedFeatures"
end

defmodule Google.Protobuf.FeatureSetDefaults do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :defaults, 1,
    repeated: true,
    type: Google.Protobuf.FeatureSetDefaults.FeatureSetEditionDefault

  field :minimum_edition, 4,
    optional: true,
    type: Google.Protobuf.Edition,
    json_name: "minimumEdition",
    enum: true

  field :maximum_edition, 5,
    optional: true,
    type: Google.Protobuf.Edition,
    json_name: "maximumEdition",
    enum: true
end

defmodule Google.Protobuf.SourceCodeInfo.Location do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :path, 1, repeated: true, type: :int32, packed: true, deprecated: false
  field :span, 2, repeated: true, type: :int32, packed: true, deprecated: false
  field :leading_comments, 3, optional: true, type: :string, json_name: "leadingComments"
  field :trailing_comments, 4, optional: true, type: :string, json_name: "trailingComments"

  field :leading_detached_comments, 6,
    repeated: true,
    type: :string,
    json_name: "leadingDetachedComments"
end

defmodule Google.Protobuf.SourceCodeInfo do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :location, 1, repeated: true, type: Google.Protobuf.SourceCodeInfo.Location

  extensions [{536_000_000, 536_000_001}]
end

defmodule Google.Protobuf.GeneratedCodeInfo.Annotation do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :path, 1, repeated: true, type: :int32, packed: true, deprecated: false
  field :source_file, 2, optional: true, type: :string, json_name: "sourceFile"
  field :begin, 3, optional: true, type: :int32
  field :end, 4, optional: true, type: :int32

  field :semantic, 5,
    optional: true,
    type: Google.Protobuf.GeneratedCodeInfo.Annotation.Semantic,
    enum: true
end

defmodule Google.Protobuf.GeneratedCodeInfo do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :annotation, 1, repeated: true, type: Google.Protobuf.GeneratedCodeInfo.Annotation
end

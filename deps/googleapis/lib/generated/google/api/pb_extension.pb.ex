defmodule Google.Api.PbExtension do
  use Protobuf, protoc_gen_elixir_version: "0.15.0"

  extend Google.Protobuf.MethodOptions, :http, 72_295_728,
    optional: true,
    type: Google.Api.HttpRule

  extend Google.Protobuf.MethodOptions, :method_signature, 1051,
    repeated: true,
    type: :string,
    json_name: "methodSignature"

  extend Google.Protobuf.ServiceOptions, :default_host, 1049,
    optional: true,
    type: :string,
    json_name: "defaultHost"

  extend Google.Protobuf.ServiceOptions, :oauth_scopes, 1050,
    optional: true,
    type: :string,
    json_name: "oauthScopes"

  extend Google.Protobuf.ServiceOptions, :api_version, 525_000_001,
    optional: true,
    type: :string,
    json_name: "apiVersion"

  extend Google.Protobuf.FieldOptions, :field_behavior, 1052,
    repeated: true,
    type: Google.Api.FieldBehavior,
    json_name: "fieldBehavior",
    enum: true,
    packed: false,
    deprecated: false

  extend Google.Protobuf.FieldOptions, :field_info, 291_403_980,
    optional: true,
    type: Google.Api.FieldInfo,
    json_name: "fieldInfo"

  extend Google.Protobuf.FieldOptions, :resource_reference, 1055,
    optional: true,
    type: Google.Api.ResourceReference,
    json_name: "resourceReference"

  extend Google.Protobuf.FileOptions, :resource_definition, 1053,
    repeated: true,
    type: Google.Api.ResourceDescriptor,
    json_name: "resourceDefinition"

  extend Google.Protobuf.MessageOptions, :resource, 1053,
    optional: true,
    type: Google.Api.ResourceDescriptor

  extend Google.Protobuf.EnumOptions, :enum_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "enumVisibility"

  extend Google.Protobuf.EnumValueOptions, :value_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "valueVisibility"

  extend Google.Protobuf.FieldOptions, :field_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "fieldVisibility"

  extend Google.Protobuf.MessageOptions, :message_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "messageVisibility"

  extend Google.Protobuf.MethodOptions, :method_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "methodVisibility"

  extend Google.Protobuf.ServiceOptions, :api_visibility, 72_295_727,
    optional: true,
    type: Google.Api.VisibilityRule,
    json_name: "apiVisibility"
end

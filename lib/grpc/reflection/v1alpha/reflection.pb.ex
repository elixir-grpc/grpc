defmodule Grpc.Reflection.V1alpha.ServerReflectionRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "host",
          label: :LABEL_OPTIONAL,
          name: "host",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "fileByFilename",
          label: :LABEL_OPTIONAL,
          name: "file_by_filename",
          number: 3,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "fileContainingSymbol",
          label: :LABEL_OPTIONAL,
          name: "file_containing_symbol",
          number: 4,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "fileContainingExtension",
          label: :LABEL_OPTIONAL,
          name: "file_containing_extension",
          number: 5,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ExtensionRequest"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "allExtensionNumbersOfType",
          label: :LABEL_OPTIONAL,
          name: "all_extension_numbers_of_type",
          number: 6,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "listServices",
          label: :LABEL_OPTIONAL,
          name: "list_services",
          number: 7,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        }
      ],
      name: "ServerReflectionRequest",
      nested_type: [],
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          __unknown_fields__: [],
          name: "message_request",
          options: nil
        }
      ],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  oneof :message_request, 0

  field :host, 1, type: :string
  field :file_by_filename, 3, type: :string, json_name: "fileByFilename", oneof: 0
  field :file_containing_symbol, 4, type: :string, json_name: "fileContainingSymbol", oneof: 0

  field :file_containing_extension, 5,
    type: Grpc.Reflection.V1alpha.ExtensionRequest,
    json_name: "fileContainingExtension",
    oneof: 0

  field :all_extension_numbers_of_type, 6,
    type: :string,
    json_name: "allExtensionNumbersOfType",
    oneof: 0

  field :list_services, 7, type: :string, json_name: "listServices", oneof: 0
end

defmodule Grpc.Reflection.V1alpha.ExtensionRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "containingType",
          label: :LABEL_OPTIONAL,
          name: "containing_type",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "extensionNumber",
          label: :LABEL_OPTIONAL,
          name: "extension_number",
          number: 2,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_INT32,
          type_name: nil
        }
      ],
      name: "ExtensionRequest",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :containing_type, 1, type: :string, json_name: "containingType"
  field :extension_number, 2, type: :int32, json_name: "extensionNumber"
end

defmodule Grpc.Reflection.V1alpha.ServerReflectionResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "validHost",
          label: :LABEL_OPTIONAL,
          name: "valid_host",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "originalRequest",
          label: :LABEL_OPTIONAL,
          name: "original_request",
          number: 2,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ServerReflectionRequest"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "fileDescriptorResponse",
          label: :LABEL_OPTIONAL,
          name: "file_descriptor_response",
          number: 4,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.FileDescriptorResponse"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "allExtensionNumbersResponse",
          label: :LABEL_OPTIONAL,
          name: "all_extension_numbers_response",
          number: 5,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ExtensionNumberResponse"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "listServicesResponse",
          label: :LABEL_OPTIONAL,
          name: "list_services_response",
          number: 6,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ListServiceResponse"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "errorResponse",
          label: :LABEL_OPTIONAL,
          name: "error_response",
          number: 7,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ErrorResponse"
        }
      ],
      name: "ServerReflectionResponse",
      nested_type: [],
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          __unknown_fields__: [],
          name: "message_response",
          options: nil
        }
      ],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  oneof :message_response, 0

  field :valid_host, 1, type: :string, json_name: "validHost"

  field :original_request, 2,
    type: Grpc.Reflection.V1alpha.ServerReflectionRequest,
    json_name: "originalRequest"

  field :file_descriptor_response, 4,
    type: Grpc.Reflection.V1alpha.FileDescriptorResponse,
    json_name: "fileDescriptorResponse",
    oneof: 0

  field :all_extension_numbers_response, 5,
    type: Grpc.Reflection.V1alpha.ExtensionNumberResponse,
    json_name: "allExtensionNumbersResponse",
    oneof: 0

  field :list_services_response, 6,
    type: Grpc.Reflection.V1alpha.ListServiceResponse,
    json_name: "listServicesResponse",
    oneof: 0

  field :error_response, 7,
    type: Grpc.Reflection.V1alpha.ErrorResponse,
    json_name: "errorResponse",
    oneof: 0
end

defmodule Grpc.Reflection.V1alpha.FileDescriptorResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "fileDescriptorProto",
          label: :LABEL_REPEATED,
          name: "file_descriptor_proto",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_BYTES,
          type_name: nil
        }
      ],
      name: "FileDescriptorResponse",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :file_descriptor_proto, 1, repeated: true, type: :bytes, json_name: "fileDescriptorProto"
end

defmodule Grpc.Reflection.V1alpha.ExtensionNumberResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "baseTypeName",
          label: :LABEL_OPTIONAL,
          name: "base_type_name",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "extensionNumber",
          label: :LABEL_REPEATED,
          name: "extension_number",
          number: 2,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_INT32,
          type_name: nil
        }
      ],
      name: "ExtensionNumberResponse",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :base_type_name, 1, type: :string, json_name: "baseTypeName"
  field :extension_number, 2, repeated: true, type: :int32, json_name: "extensionNumber"
end

defmodule Grpc.Reflection.V1alpha.ListServiceResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "service",
          label: :LABEL_REPEATED,
          name: "service",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".grpc.reflection.v1alpha.ServiceResponse"
        }
      ],
      name: "ListServiceResponse",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :service, 1, repeated: true, type: Grpc.Reflection.V1alpha.ServiceResponse
end

defmodule Grpc.Reflection.V1alpha.ServiceResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "name",
          label: :LABEL_OPTIONAL,
          name: "name",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        }
      ],
      name: "ServiceResponse",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :name, 1, type: :string
end

defmodule Grpc.Reflection.V1alpha.ErrorResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.10.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "errorCode",
          label: :LABEL_OPTIONAL,
          name: "error_code",
          number: 1,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_INT32,
          type_name: nil
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "errorMessage",
          label: :LABEL_OPTIONAL,
          name: "error_message",
          number: 2,
          oneof_index: nil,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_STRING,
          type_name: nil
        }
      ],
      name: "ErrorResponse",
      nested_type: [],
      oneof_decl: [],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  field :error_code, 1, type: :int32, json_name: "errorCode"
  field :error_message, 2, type: :string, json_name: "errorMessage"
end

defmodule Grpc.Reflection.V1alpha.ServerReflection.Service do
  @moduledoc false
  use GRPC.Service,
    name: "grpc.reflection.v1alpha.ServerReflection",
    protoc_gen_elixir_version: "0.10.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      __unknown_fields__: [],
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: true,
          input_type: ".grpc.reflection.v1alpha.ServerReflectionRequest",
          name: "ServerReflectionInfo",
          options: nil,
          output_type: ".grpc.reflection.v1alpha.ServerReflectionResponse",
          server_streaming: true
        }
      ],
      name: "ServerReflection",
      options: nil
    }
  end

  rpc :ServerReflectionInfo,
      stream(Grpc.Reflection.V1alpha.ServerReflectionRequest),
      stream(Grpc.Reflection.V1alpha.ServerReflectionResponse)
end

defmodule Grpc.Reflection.V1alpha.ServerReflection.Stub do
  @moduledoc false
  use GRPC.Stub, service: Grpc.Reflection.V1alpha.ServerReflection.Service
end

defmodule Google.Longrunning.PbExtension do
  use Protobuf, protoc_gen_elixir_version: "0.15.0"

  extend Google.Protobuf.MethodOptions, :operation_info, 1049,
    optional: true,
    type: Google.Longrunning.OperationInfo,
    json_name: "operationInfo"
end

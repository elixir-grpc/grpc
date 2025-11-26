defmodule Google.Longrunning.Operation do
  @moduledoc """
  This resource represents a long-running operation that is the result of a
  network API call.
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  oneof :result, 0

  field :name, 1, type: :string
  field :metadata, 2, type: Google.Protobuf.Any
  field :done, 3, type: :bool
  field :error, 4, type: Google.Rpc.Status, oneof: 0
  field :response, 5, type: Google.Protobuf.Any, oneof: 0
end

defmodule Google.Longrunning.GetOperationRequest do
  @moduledoc """
  The request message for
  [Operations.GetOperation][google.longrunning.Operations.GetOperation].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Google.Longrunning.ListOperationsRequest do
  @moduledoc """
  The request message for
  [Operations.ListOperations][google.longrunning.Operations.ListOperations].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 4, type: :string
  field :filter, 1, type: :string
  field :page_size, 2, type: :int32, json_name: "pageSize"
  field :page_token, 3, type: :string, json_name: "pageToken"
end

defmodule Google.Longrunning.ListOperationsResponse do
  @moduledoc """
  The response message for
  [Operations.ListOperations][google.longrunning.Operations.ListOperations].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :operations, 1, repeated: true, type: Google.Longrunning.Operation
  field :next_page_token, 2, type: :string, json_name: "nextPageToken"
end

defmodule Google.Longrunning.CancelOperationRequest do
  @moduledoc """
  The request message for
  [Operations.CancelOperation][google.longrunning.Operations.CancelOperation].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Google.Longrunning.DeleteOperationRequest do
  @moduledoc """
  The request message for
  [Operations.DeleteOperation][google.longrunning.Operations.DeleteOperation].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Google.Longrunning.WaitOperationRequest do
  @moduledoc """
  The request message for
  [Operations.WaitOperation][google.longrunning.Operations.WaitOperation].
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :name, 1, type: :string
  field :timeout, 2, type: Google.Protobuf.Duration
end

defmodule Google.Longrunning.OperationInfo do
  @moduledoc """
  A message representing the message types used by a long-running operation.

  Example:

      rpc Export(ExportRequest) returns (google.longrunning.Operation) {
        option (google.longrunning.operation_info) = {
          response_type: "ExportResponse"
          metadata_type: "ExportMetadata"
        };
      }
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :response_type, 1, type: :string, json_name: "responseType"
  field :metadata_type, 2, type: :string, json_name: "metadataType"
end

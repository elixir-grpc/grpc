defmodule Transcode.MessageOut do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :response, 1, type: Transcode.Message
end

defmodule Transcode.GetMessageRequest do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Transcode.Message do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
  field :text, 2, type: :string
end

defmodule Transcode.NestedMessageRequest do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :message, 1, type: Transcode.GetMessageRequest
end

defmodule Transcode.Messaging.Service do
  @moduledoc false

  use GRPC.Service, name: "transcode.Messaging", protoc_gen_elixir_version: "0.14.1"

  rpc :GetMessage, Transcode.GetMessageRequest, Transcode.Message

  rpc :StreamMessages, Transcode.GetMessageRequest, stream(Transcode.Message)

  rpc :GetMessageWithSubPath, Transcode.GetMessageRequest, Transcode.Message

  rpc :GetMessageWithQuery, Transcode.GetMessageRequest, Transcode.Message

  rpc :GetMessageWithFieldPath, Transcode.NestedMessageRequest, Transcode.Message

  rpc :CreateMessage, Transcode.Message, Transcode.Message

  rpc :GetMessageWithResponseBody, Transcode.GetMessageRequest, Transcode.MessageOut

  rpc :CreateMessageWithNestedBody, Transcode.NestedMessageRequest, Transcode.Message

  rpc :GetMessageWithSubpathQuery, Transcode.NestedMessageRequest, Transcode.Message
end

defmodule Transcode.Messaging.Stub do
  @moduledoc false

  use GRPC.Stub, service: Transcode.Messaging.Service
end

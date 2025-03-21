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

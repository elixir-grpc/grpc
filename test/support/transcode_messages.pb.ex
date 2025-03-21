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

  rpc(:GetMessage, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/messages/{name}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:StreamMessages, Transcode.GetMessageRequest, stream(Transcode.Message), %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/messages/stream/{name}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:GetMessageWithSubPath, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/{name=}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:GetMessageWithQuery, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/messages"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:GetMessageWithFieldPath, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/messages/fieldpath/{message.name}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:CreateMessage, Transcode.Message, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "*",
        additional_bindings: [],
        response_body: "",
        pattern: {:post, "/v1/messages"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:GetMessageWithResponseBody, Transcode.GetMessageRequest, Transcode.MessageOut, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "response",
        pattern: {:get, "/v1/messages/response_body/{name}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:CreateMessageWithNestedBody, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "message",
        additional_bindings: [],
        response_body: "",
        pattern: {:post, "/v1/messages/nested"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:GetMessageWithSubpathQuery, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/messages/nested"},
        __unknown_fields__: []
      }
    }
  })
end

defmodule Transcode.Messaging.Stub do
  @moduledoc false

  use GRPC.Stub, service: Transcode.Messaging.Service
end

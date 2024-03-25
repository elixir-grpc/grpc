defmodule Transcode.MessageOut do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :response, 1, type: Transcode.Message
end

defmodule Transcode.GetMessageRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Transcode.Message do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :text, 2, type: :string
end

defmodule Transcode.NestedMessageRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :message, 1, type: Transcode.GetMessageRequest
end

defmodule Transcode.Messaging.Service do
  @moduledoc false
  use GRPC.Service, name: "transcode.Messaging", protoc_gen_elixir_version: "0.11.0"

  rpc(:GetMessage, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages/{name}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:StreamMessages, Transcode.GetMessageRequest, stream(Transcode.Message), %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages/stream/{name}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:GetMessageWithSubPath, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/{name=}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:GetMessageWithQuery, Transcode.GetMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:GetMessageWithFieldPath, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages/fieldpath/{message.name}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:CreateMessage, Transcode.Message, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "*",
        pattern: {:post, "/v1/messages"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:GetMessageWithResponseBody, Transcode.GetMessageRequest, Transcode.MessageOut, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages/response_body/{name}"},
        response_body: "response",
        selector: ""
      }
    }
  })

  rpc(:CreateMessageWithNestedBody, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "message",
        pattern: {:post, "/v1/messages/nested"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:GetMessageWithSubpathQuery, Transcode.NestedMessageRequest, Transcode.Message, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/messages/nested"},
        response_body: "",
        selector: ""
      }
    }
  })
end

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

defmodule Transcode.Messaging.Stub do
  @moduledoc false

  use GRPC.Stub, service: Transcode.Messaging.Service
end

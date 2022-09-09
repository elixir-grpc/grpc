defmodule Helloworld.Greeter.Service do
  @moduledoc false

  use GRPC.Service, name: "helloworld.Greeter", protoc_gen_elixir_version: "0.11.0"

  rpc(:SayHello, Helloworld.HelloRequest, Helloworld.HelloReply, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "",
        pattern: {:get, "/v1/greeter/{name}"},
        response_body: "",
        selector: ""
      }
    }
  })

  rpc(:SayHelloFrom, Helloworld.HelloRequestFrom, Helloworld.HelloReply, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        __unknown_fields__: [],
        additional_bindings: [],
        body: "*",
        pattern: {:post, "/v1/greeter"},
        response_body: "",
        selector: ""
      }
    }
  })
end

defmodule Helloworld.Greeter.Stub do
  @moduledoc false

  use GRPC.Stub, service: Helloworld.Greeter.Service
end
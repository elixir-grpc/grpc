defmodule Helloworld.HelloRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Helloworld.HelloRequestFrom do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :from, 2, type: :string
end

defmodule Helloworld.HelloReply do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :message, 1, type: :string
  field :today, 2, type: Google.Protobuf.Timestamp
end

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

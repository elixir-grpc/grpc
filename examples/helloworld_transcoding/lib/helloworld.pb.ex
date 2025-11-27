defmodule Helloworld.HelloRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
end

defmodule Helloworld.HelloRequestFrom do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
  field :from, 2, type: :string
end

defmodule Helloworld.HelloReply do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :message, 1, type: :string
  field :today, 2, type: Google.Protobuf.Timestamp
end

defmodule Helloworld.Greeter.Service do
  @moduledoc false
  use GRPC.Service, name: "helloworld.Greeter", protoc_gen_elixir_version: "0.14.1"

  rpc(:SayHello, Helloworld.HelloRequest, Helloworld.HelloReply, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "",
        additional_bindings: [],
        response_body: "",
        pattern: {:get, "/v1/greeter/{name}"},
        __unknown_fields__: []
      }
    }
  })

  rpc(:SayHelloFrom, Helloworld.HelloRequestFrom, Helloworld.HelloReply, %{
    http: %{
      type: Google.Api.PbExtension,
      value: %Google.Api.HttpRule{
        selector: "",
        body: "*",
        additional_bindings: [],
        response_body: "",
        pattern: {:post, "/v1/greeter"},
        __unknown_fields__: []
      }
    }
  })
end

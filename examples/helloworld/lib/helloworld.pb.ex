defmodule Helloworld.HelloRequest do
  use Protobuf

  @type t :: %__MODULE__{
    name: String.t
  }
  defstruct [:name]

  field :name, 1, optional: true, type: :string
end

defmodule Helloworld.HelloReply do
  use Protobuf

  @type t :: %__MODULE__{
    message: String.t
  }
  defstruct [:message]

  field :message, 1, optional: true, type: :string
end

defmodule Helloworld.Greeter.Service do
  use GRPC.Service, name: "helloworld.Greeter"

  rpc :SayHello, Helloworld.HelloRequest, Helloworld.HelloReply
end

defmodule Helloworld.Greeter.Stub do
  use GRPC.Stub, service: Helloworld.Greeter.Service
end

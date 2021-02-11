defmodule Helloworld.HelloRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t()
        }
  defstruct [:name]

  field :name, 1, type: :string
end

defmodule Helloworld.HelloReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          message: String.t()
        }
  defstruct [:message]

  field :message, 1, type: :string
end

defmodule Helloworld.HeaderRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{}
  defstruct []
end

defmodule Helloworld.HeaderReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          authorization: String.t()
        }
  defstruct [:authorization]

  field :authorization, 1, type: :string
end

defmodule Helloworld.Greeter.Service do
  @moduledoc false
  use GRPC.Service, name: "helloworld.Greeter"

  rpc :SayHello, Helloworld.HelloRequest, Helloworld.HelloReply
  rpc :CheckHeaders, Helloworld.HeaderRequest, Helloworld.HeaderReply
end

defmodule Helloworld.Greeter.Stub do
  @moduledoc false
  use GRPC.Stub, service: Helloworld.Greeter.Service
end

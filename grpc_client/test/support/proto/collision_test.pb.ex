defmodule CollisionTest.Request do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :data, 1, type: :string
end

defmodule CollisionTest.Response do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :result, 1, type: :string
end

defmodule CollisionTest.CollisionService.Service do
  @moduledoc false

  use GRPC.Service, name: "collision_test.CollisionService", protoc_gen_elixir_version: "0.14.1"

  rpc :process_data, CollisionTest.Request, CollisionTest.Response

  rpc :ProcessData, CollisionTest.Request, CollisionTest.Response

  rpc :GetUser, CollisionTest.Request, CollisionTest.Response
end

defmodule CollisionTest.CollisionService.Stub do
  @moduledoc false

  use GRPC.Stub, service: CollisionTest.CollisionService.Service, warn_on_collision: false
end

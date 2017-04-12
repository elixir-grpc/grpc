defmodule GRPC.Proto do
  defstruct [package: nil, services: [], messages: []]

  @moduledoc false

  defmodule Service do
    @moduledoc false
    defstruct [name: nil, rpcs: []]
  end

  defmodule Message do
    @moduledoc false
    defstruct []
  end
end

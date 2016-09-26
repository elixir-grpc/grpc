defmodule GRPC.Proto do
  defstruct [package: nil, services: [], messages: []]

  defmodule Service do
    defstruct [name: nil, rpcs: []]
  end

  defmodule Message do
    defstruct []
  end
end

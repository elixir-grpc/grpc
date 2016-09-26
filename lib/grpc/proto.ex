defmodule GRPC.Proto do
  defstruct [package: "", services: [], messages: []]

  defmodule Service do
    defstruct [name: nil, rpcs: []]
  end

  defmodule Message do
    defstruct []
  end
end

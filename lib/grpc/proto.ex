defmodule GRPC.Proto do
  defstruct [package: nil, services: [], messages: []]

  @moduledoc """
  Used in templates of generator
  """

  defmodule Service do
    defstruct [name: nil, rpcs: []]
  end

  defmodule Message do
    defstruct []
  end
end

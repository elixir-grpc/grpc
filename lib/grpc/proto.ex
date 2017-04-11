defmodule GRPC.Proto do
  @moduledoc """
    GRPC.Proto 
  """
  defstruct [package: nil, services: [], messages: []]

  defmodule Service do
    @moduledoc """
      Service 
    """
    defstruct [name: nil, rpcs: []]
  end

  defmodule Message do
    @moduledoc """
      Message 
    """ 
    defstruct []
  end
end

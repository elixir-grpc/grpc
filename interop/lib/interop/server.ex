defmodule Interop.Server do
  @moduledoc false
  use GRPC.Server, service: Grpc.Testing.TestService.Service

  def empty_call(_, _stream) do
    Grpc.Testing.Empty.new()
  end
end

defmodule Interop.Client do
  def connect(host, port) do
    {:ok, ch} = GRPC.Stub.connect(host, port, [])
    ch
  end

  def empty_unary!(ch) do
    empty = Grpc.Testing.Empty.new()
    {:ok, ^empty} = Grpc.Testing.TestService.Stub.empty_call(ch, empty)
  end
end

defmodule GRPC.Core.CompletionQueue do
  import GRPC.Nifs

  def create do
    completion_queue_create()
  end

end

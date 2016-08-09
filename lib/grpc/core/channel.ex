defmodule GRPC.Core.Channel do
  import GRPC.Nifs

  def create(target, channel_args, credentials) do
    channel_create(target, channel_args, credentials)
  end
end

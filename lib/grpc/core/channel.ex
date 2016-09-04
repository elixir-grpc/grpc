defmodule GRPC.Core.Channel do
  import GRPC.Nifs

  def create(target, channel_args, credentials) when is_binary(target) do
    target = String.to_charlist(target)
    create(target, channel_args, credentials)
  end
  def create(target, channel_args, credentials) do
    channel_create(target, channel_args, credentials)
  end

end

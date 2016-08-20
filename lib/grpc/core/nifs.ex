defmodule GRPC.Nifs do
  @on_load { :init, 0 }

  app = Mix.Project.config[:app]

  def init do
    path = :filename.join(:code.priv_dir(unquote(app)), "grpc_c")
    :ok = :erlang.load_nif(path, 0)
  end

  def completion_queue_create() do
    not_impl!
  end

  def channel_create(a, b, c) do
    not_impl!
  end

  def call_create(channel, parent_call, propagation_mask, completion_queue, method, host, deadline) do
    not_impl!
  end

  defp not_impl! do
    exit(:nif_library_not_loaded)
  end

end

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

  def channel_create(_target, _channel_args, _credentials) do
    not_impl!
  end

  def call_create(_channel, _parent_call, _propagation_mask, _completion_queue, _method, _host, _deadline) do
    not_impl!
  end

  def call_run_batch(_call, _ops, _tag) do
    not_impl!
  end

  def call_finish_batch(_call, _completion_queue, _tag, _timeout) do
    not_impl!
  end

  defp not_impl! do
    exit(:nif_library_not_loaded)
  end

end

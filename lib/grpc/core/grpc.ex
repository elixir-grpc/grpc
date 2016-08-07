defmodule GRPC.Nifs do
  @on_load { :init, 0 }

  app = Mix.Project.config[:app]

  def init do
    path = :filename.join(:code.priv_dir(unquote(app)), "grpc_c")
    :ok = :erlang.load_nif(path, 0)
  end

  def completion_queue_create() do
    exit(:nif_library_not_loaded)
  end

end

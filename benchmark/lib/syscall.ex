defmodule Benchmark.Syscall do
  @on_load {:init, 0}

  # %{stime: {sec, usec}, utime: {sec, usec}}
  def init do
    path = :filename.join(:code.priv_dir(:benchmark), ~c"syscall")
    :ok = :erlang.load_nif(path, 0)
  end

  def getrusage() do
    not_impl!()
  end

  defp not_impl! do
    exit(:nif_library_not_loaded)
  end
end

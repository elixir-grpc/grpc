defmodule Benchmark.Syscall do
  @on_load {:init, 0}

  # %{stime: {sec, usec}, utime: {sec, usec}}
  def init do
    :ok = :erlang.load_nif("syscall", 0)
  end

  def getrusage() do
    not_impl!()
  end

  defp not_impl! do
    exit(:nif_library_not_loaded)
  end
end

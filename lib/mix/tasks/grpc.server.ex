defmodule Mix.Tasks.Grpc.Server do
  use Mix.Task

  @shortdoc "Starts applications and their servers"

  @moduledoc """
  Starts the application by configuring `start_server` to true.

  The `--no-halt` flag is automatically added.
  """
  def run(args) do
    Application.put_env(:grpc, :start_server, true, persistent: true)
    Mix.Task.run "run", run_args() ++ args
  end

  defp run_args do
    if iex_running?(), do: [], else: ["--no-halt"]
  end

  defp iex_running? do
    Code.ensure_loaded?(IEx) and IEx.started?
  end
end

defmodule GRPC.GRPCRoot do
  use Mix.Project

  @version "1.0.0-rc.1"

  def project do
    [
      app: :grpc_root,
      version: @version,
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp deps do
    [
      {:grpc_server, path: "grpc_server"},
    ]
  end
 
  defp aliases do
    [
      setup: cmd("deps.get"),
      compile: cmd("compile"),
      test: cmd("test"),
      hex_build: cmd("hex.build"),
      hex_docs: cmd("hex.docs")
    ]
  end

  defp cmd(command) do
    ansi = IO.ANSI.enabled?()
    base = ["--erl", "-elixir ansi_enabled #{ansi}", "-S", "mix", command]

    for app <- ~w(grpc_core grpc_server grpc_client) do
      fn args ->
        {_, res} = System.cmd("elixir", base ++ args, into: IO.binstream(:stdio, :line), cd: app)

        if res > 0 do
          System.at_exit(fn _ -> exit({:shutdown, 1}) end)
        end
      end
    end
  end

end

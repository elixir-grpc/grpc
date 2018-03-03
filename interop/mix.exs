defmodule Interop.MixProject do
  use Mix.Project

  def project do
    [
      app: :interop,
      version: "0.1.0",
      elixir: "~> 1.4",
      start_permanent: true,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Interop.App, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:grpc, github: "tony612/grpc-elixir", branch: "add-gun"},
      {:gun, ">= 1.0.0-pre.5"}
    ]
  end
end

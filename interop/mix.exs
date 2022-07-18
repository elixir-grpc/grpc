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
      {:grpc, path: "..", override: true},
      {:grpc_prometheus, ">= 0.1.0"},
      {:grpc_statsd, "~> 0.1.0"},
      {:statix, ">= 1.2.1"},
      {:extrace, "~> 0.2"},
      {:prometheus, "~> 4.0", override: true}
    ]
  end
end

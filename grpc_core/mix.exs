defmodule GRPC.Core.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-grpc/grpc"
  @version "1.0.0-alfa"

  def project do
    [
      app: :grpc_core,
      version: @version,
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Core gRPC types, codecs, and utilities for Elixir",
      package: package(),
      docs: docs(),
      name: "gRPC Core",
      source_url: @source_url
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:protobuf, "~> 0.14"},
      {:jason, ">= 0.0.0"},
      {:telemetry, "~> 1.0"},
      {:googleapis, "~> 0.1.0"},
      {:ex_doc, "~> 0.39", only: [:dev, :docs], runtime: false},
      {:makeup, "~> 1.2.1", only: [:dev, :docs], runtime: false},
      {:makeup_syntect, "~> 0.1", only: [:dev, :docs], runtime: false}
    ]
  end

  defp package do
    %{
      maintainers: ["Adriano Santos", "Dave Lucia", "Bing Han", "Paulo Valente"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ~w(lib mix.exs README.md LICENSE)
    }
  end

  defp docs do
    [
      main: "readme",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: ["README.md"]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end

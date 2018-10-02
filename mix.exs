defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.3.0-alpha.2"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: "The Elixir implementation of gRPC",
      docs: [
        extras: ["README.md"],
        main: "readme",
        source_ref: "v#{@version}",
        source_url: "https://github.com/tony612/grpc-elixir"
      ]
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:protobuf, "~> 0.5"},
      # FIXME 2018-10-02: We need features of Cowboy only available in the unreleased v2.5.0
      {:cowboy, github: "ninenines/cowboy", ref: "c998673eb009da2ea4dc0e6ef0332534cf679cc4"},
      {:gun, "~> 1.1"},
      {:cowlib, "~> 2.1", override: true},
      {:ex_doc, "~> 0.14", only: :dev},
      {:inch_ex, ">= 0.0.0", only: :docs},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false}
    ]
  end

  defp package do
    %{
      maintainers: ["Bing Han"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/tony612/grpc-elixir"},
      files: ~w(mix.exs README.md lib src config LICENSE .formatter.exs)
    }
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end

defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.6.0"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.11",
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
        source_url: "https://github.com/elixir-grpc/grpc"
      ],
      dialyzer: [
        plt_add_deps: :apps_tree,
        plt_add_apps: [:iex, :mix, :ex_unit],
        list_unused_filters: true,
        plt_file: {:no_warn, "_build/#{Mix.env()}/plts/dialyzer.plt"}
      ],
      xref: [exclude: [IEx]]
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
      {:cowboy, "~> 2.10"},
      {:gun, "~> 2.0"},
      {:mint, "~> 1.5.0"},
      {:cowlib, "~> 2.12"},
      {:protobuf, "~> 0.11", only: [:dev, :test]},
      {:ex_doc, "~> 0.29", only: :dev},
      {:dialyxir, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:ex_parameterized, "~> 1.3.7", only: :test},
      {:telemetry, "~> 1.0"}
    ]
  end

  defp package do
    %{
      maintainers: ["Bing Han", "Paulo Valente"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/grpc"},
      files: ~w(mix.exs README.md lib src config LICENSE .formatter.exs)
    }
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end

defmodule HPAX.MixProject do
  use Mix.Project

  @version "1.0.3"
  @repo_url "https://github.com/elixir-mint/hpax"

  def project do
    [
      app: :hpax,
      version: @version,
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Tests
      test_coverage: [tool: ExCoveralls],

      # Hex
      package: package(),
      description: "Implementation of the HPACK protocol (RFC 7541) for Elixir",

      # Docs
      name: "HPAX",
      docs: [
        source_ref: "v#{@version}",
        source_url: @repo_url
      ]
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.34", only: :dev},
      {:hpack, ">= 0.0.0", hex: :hpack_erl, only: :test},
      {:stream_data, "~> 1.0", only: [:dev, :test]},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:castore, "~> 1.0", only: :test}
    ]
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @repo_url}
    ]
  end
end

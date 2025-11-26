defmodule Googleapis.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :googleapis,
      version: @version,
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: "The Elixir googleapis package, containing generated code for Google APIs.",
      docs: [
        extras: ["README.md"],
        main: "readme",
        source_ref: "v#{@version}",
        source_url: "https://github.com/elixir-grpc/googleapis"
      ],
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:protobuf, "~> 0.12"},
      {:ex_doc, "~> 0.30", only: :dev, runtime: false}
    ]
  end

  defp package do
    %{
      maintainers: ["Adriano Santos", "Dave Lucia", "Bing Han", "Paulo Valente", "Yordis Prieto"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/googleapis"},
      files: ~w(mix.exs README.md lib LICENSE .formatter.exs)
    }
  end
end

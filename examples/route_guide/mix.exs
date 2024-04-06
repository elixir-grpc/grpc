defmodule RouteGuide.Mixfile do
  use Mix.Project

  def project do
    [
      app: :route_guide,
      version: "0.1.0",
      elixir: "~> 1.11",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [mod: {Routeguide.App, []}, applications: [:logger, :grpc, :protobuf, :jason]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:grpc, path: "../../"},
      {:protobuf, "~> 0.11"},
      {:jason, "~> 1.2"},
    ]
  end
end

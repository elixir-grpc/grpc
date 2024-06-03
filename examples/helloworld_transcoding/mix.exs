defmodule Helloworld.Mixfile do
  use Mix.Project

  def project do
    [
      app: :helloworld,
      version: "0.1.0",
      elixir: "~> 1.4",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [mod: {HelloworldApp, []}, applications: [:logger, :grpc]]
  end

  defp deps do
    [
      {:grpc, path: "../../"},
      {:protobuf, "~> 0.11.0"},
      {:protobuf_generate, "~> 0.1.1", only: [:dev, :test]},
      {:jason, "~> 1.3.0"},
      {:google_protos, "~> 0.3.0"},
    ]
  end
end

defmodule Benchmark.MixProject do
  use Mix.Project
  @version "1.0.0-rc.1"

  def project do
    [
      app: :benchmark,
      version: @version,
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Benchmark.Application, []}
    ]
  end

  defp deps do
    [
      {:grpc_server, path: "../grpc_server"},
      {:grpc_client, path: "../grpc_client"},
      {:protobuf, "~> 0.14"}
    ]
  end
end

defmodule Benchmark.MixProject do
  use Mix.Project

  def project do
    [
      app: :benchmark,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
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

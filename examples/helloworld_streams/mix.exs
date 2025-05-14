defmodule HelloworldStreams.MixProject do
  use Mix.Project

  def project do
    [
      app: :helloworld_streams,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {HelloworldStreams.Application, []}
    ]
  end

  defp deps do
    [
      {:grpc, path: "../../", override: true},
      {:protobuf, "~> 0.14"},
      {:grpc_reflection, "~> 0.1"}
    ]
  end
end

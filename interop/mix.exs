defmodule Interop.MixProject do
  use Mix.Project

  @version "1.0.0-rc.1"

  def project do
    [
      app: :interop,
      version: @version,
      elixir: "~> 1.4",
      start_permanent: true,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Interop.App, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:grpc_server, path: "../grpc_server", override: true},
      {:grpc_client, path: "../grpc_client", override: true},
      {:protobuf, "~> 0.14"},
      {:grpc_statsd, "~> 0.1.0"},
      {:statix, ">= 1.2.1"},
      {:extrace, "~> 0.2"}
    ]
  end
end

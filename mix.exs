defmodule GRPC.Mixfile do
  use Mix.Project

  def project do
    [app: :grpc,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: [:elixir_make] ++ Mix.compilers,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:exprotobuf, "~> 1.1.0"},
     {:elixir_make, "~> 0.3.0"},
     {:grpc_core, github: "tony612/grpc-core", tag: "v1.0.0", app: false, compile: false}
    ]
  end
end

defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [app: :grpc,
     version: @version,
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:lager, :logger, :exprotobuf, :chatterbox, :cowboy]]
  end

  defp deps do
    [{:exprotobuf, "~> 1.2.0"},
     {:chatterbox, github: "joedevivo/chatterbox"},
     # TODO
     # https://github.com/ninenines/cowboy/pull/1020
     {:cowboy, github: "tony612/cowboy", branch: "trailers-support"},
     {:credo, "~> 0.7", only: [:dev, :test]}
    ]
  end
end

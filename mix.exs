defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [app: :grpc,
     version: @version,
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),

     package: package(),
     description: "The Elixir implementation of gRPC",
     docs: [extras: ["README.md"], main: "readme",
         source_ref: "v#{@version}",
         source_url: "https://github.com/tony612/grpc-elixir"]]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:lager, :logger, :exprotobuf, :chatterbox, :cowboy]]
  end

  defp deps do
    [{:exprotobuf, "~> 1.2.0"},
     # TODO
     # https://github.com/joedevivo/chatterbox/issues/57
     {:chatterbox, github: "tony612/chatterbox", branch: "my-fix"},
     # TODO
     # https://github.com/ninenines/cowboy/pull/1020
     {:cowboy, github: "tony612/cowboy", branch: "trailers-support"},
     {:ex_doc, "~> 0.14", only: :dev},
     {:inch_ex, only: :docs}
    ]
  end

  defp package do
    %{maintainers: ["Tony Han"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/tony612/grpc-elixir"}}
  end
end

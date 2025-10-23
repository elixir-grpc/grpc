defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.11.3"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      aliases: aliases(),
      description: "The Elixir implementation of gRPC",
      docs: [
        extras: ["README.md"],
        main: "readme",
        source_ref: "v#{@version}",
        source_url: "https://github.com/elixir-grpc/grpc"
      ],
      xref: [exclude: [IEx]]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  def escript do
    [main_module: GRPC.Protoc.CLI, name: "protoc-gen-grpc_elixir"]
  end

  defp deps do
    [
      {:googleapis, "~> 0.1.0"},
      {:cowboy, "~> 2.10"},
      {:flow, "~> 1.2"},
      {:gun, "~> 2.0"},
      {:jason, ">= 0.0.0"},
      {:cowlib, "~> 2.12"},
      {:castore, "~> 0.1 or ~> 1.0", optional: true},
      {:protobuf, "~> 0.14"},
      {:protobuf_generate, "~> 0.1.1", only: [:dev, :test]},
      {:mint, "~> 1.5"},
      {:ex_doc, "~> 0.29", only: :dev},
      {:ex_parameterized, "~> 1.3.7", only: :test},
      {:mox, "~> 1.2", only: :test},
      {:telemetry, "~> 1.0"}
    ]
  end

  defp package do
    %{
      maintainers: ["Adriano Santos", "Dave Lucia", "Bing Han", "Paulo Valente"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/grpc"},
      files: ~w(mix.exs README.md lib src config priv/templates LICENSE .formatter.exs)
    }
  end

  defp aliases do
    [
      gen_test_protos: &gen_test_protos/1
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp gen_test_protos(_args) do
    elixir_out = "test/support"

    cmd = ~s(mix protobuf.generate \
      --include-path=test/support \
      --output-path="#{elixir_out}" \
      --plugin=ProtobufGenerate.Plugins.GRPCWithOptions\
      test/support/transcode_messages.proto \
      test/support/proto/helloworld.proto \
      test/support/proto/route_guide.proto
    )

    case Mix.shell().cmd(cmd) do
      0 -> Mix.Task.rerun("format", [Path.join([elixir_out, "**", "*.pb.ex"])])
      other -> Mix.raise("'protoc' exited with non-zero status: #{other}")
    end
  end
end

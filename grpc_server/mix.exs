defmodule GRPC.Server.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-grpc/grpc_server"
  @version "1.0.0-rc.1"

  def project do
    [
      app: :grpc_server,
      version: @version,
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      description: "gRPC server implementation for Elixir",
      package: package(),
      aliases: aliases(),
      name: "gRPC Server",
      source_url: @source_url,
      xref: [exclude: [IEx]]
    ]
  end

  def application do
    [extra_applications: [:logger, :cowboy]]
  end

  def escript do
    [main_module: GRPC.Protoc.CLI, name: "protoc-gen-grpc_elixir"]
  end

  defp deps do
    [
      {:grpc_core, path: "../grpc_core"},
      # {:grpc_core, "~> 1.0"}, # Uncomment for hex release
      {:protobuf, "~> 0.14"},
      {:cowboy, "~> 2.14"},
      {:cowlib, "~> 2.14"},
      {:flow, "~> 1.2"},
      {:protobuf_generate, "~> 0.1.3", only: [:dev, :test]},
      {:ex_parameterized, "~> 1.3.7", only: :test},
      {:mox, "~> 1.2", only: :test},
      {:ex_doc, "~> 0.39", only: [:dev, :docs], runtime: false},
      {:makeup, "~> 1.2.1", only: [:dev, :docs], runtime: false},
      {:makeup_syntect, "~> 0.1", only: [:dev, :docs], runtime: false}
    ]
  end

  defp package do
    %{
      maintainers: ["Adriano Santos", "Dave Lucia", "Bing Han", "Paulo Valente"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ~w(mix.exs README.md lib src config priv/templates LICENSE .formatter.exs)
    }
  end

  defp docs() do
    [
      main: "GRPC",
      source_ref: "v#{@version}",
      source_url_pattern: "#{@source_url}/blob/v#{@version}/%{path}#L%{line}",
      extras: [
        "CHANGELOG.md",
        "guides/getting_started/quickstart.livemd",
        "guides/getting_started/stream.livemd",
        "guides/getting_started/error_handling.md",
        "guides/getting_started/codegen.md",
        "guides/cheatsheets/streams.cheatmd",
        "guides/advanced/custom_codecs.md",
        "guides/advanced/transcoding.livemd",
        "guides/advanced/cors.md",
        "guides/advanced/telemetry.livemd",
        "guides/advanced/pooling.md",
        "guides/advanced/custom_codecs.md"
      ],
      skip_undefined_reference_warnings_on: ["README.md"],
      groups_for_docs: [
        "Functions: Creation": &(&1[:type] == :creation),
        "Functions: Materializers": &(&1[:type] == :materialization),
        "Functions: Transformers": &(&1[:type] == :transforms),
        "Functions: Actions": &(&1[:type] == :actions)
      ],
      groups_for_modules: [
        "Server Core": [
          GRPC.Server,
          GRPC.Service,
          GRPC.Endpoint,
          GRPC.Server.Supervisor,
          GRPC.Server.Stream
        ],
        "Server Adapters": [
          GRPC.Server.Adapter,
          GRPC.Server.Adapters.Cowboy,
          GRPC.Server.Adapters.Cowboy.Handler,
          GRPC.Server.Adapters.Cowboy.Router
        ],
        "Server Interceptors": [
          GRPC.Server.Interceptor,
          GRPC.Server.Interceptors.Logger,
          GRPC.Server.Interceptors.CORS
        ],
        Transcoding: [
          GRPC.Server.Transcode,
          GRPC.Server.Router
        ],
        "Stream API": [
          GRPC.Stream,
          GRPC.Stream.Operators
        ],
        "Error Handling": [
          GRPC.Server.Adapters.ReportException
        ]
      ],
      groups_for_extras: [
        "Getting Started": ~r"^guides/getting_started/",
        Advanced: ~r"^guides/advanced/",
        Cheatsheets: ~r"^guides/cheatsheets/"
      ]
    ]
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
      --one-file-per-module=true \
      --generate-descriptors=true \
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

defmodule GRPC.Mixfile do
  use Mix.Project

  @source_url "https://github.com/elixir-grpc/grpc"
  @version "0.11.2"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      name: "gRPC",
      start_permanent: Mix.env() == :prod,
      package: package(),
      aliases: aliases(),
      description: "The Elixir implementation of gRPC Protocol",
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
      {:mint, "~> 1.5"},
      {:telemetry, "~> 1.0"},
      {:protobuf_generate, "~> 0.1.1", only: [:dev, :test]},
      {:ex_parameterized, "~> 1.3.7", only: :test},
      {:mox, "~> 1.2", only: :test},
      {:ex_doc, "~> 0.39", only: [:dev, :docs]},
      {:makeup, "~> 1.2.1", only: [:dev, :docs]},
      {:makeup_syntect, "~> 0.1", only: [:dev, :docs]}
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

  defp docs() do
    [
      main: "GRPC",
      source_ref: "v#{@version}",
      source_url_pattern: "#{@source_url}/blob/v#{@version}/grpc/%{path}#L%{line}",
      extras: [
        "guides/getting_started/quickstart.livemd",
        "guides/getting_started/stream.livemd",
        "guides/getting_started/error_handling.md",
        "guides/getting_started/client.md",
        "guides/getting_started/codegen.md",
        "guides/cheatsheets/streams.cheatmd",
        "guides/advanced/transcoding.livemd",
        "guides/advanced/load_balancing.md",
        "guides/advanced/cors.md",
        "guides/advanced/telemetry.livemd",
        "guides/advanced/pooling.md"
      ],
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"],
      groups_for_docs: [
        "Functions: Creation": &(&1[:type] == :creation),
        "Functions: Materializers": &(&1[:type] == :materialization),
        "Functions: Transformers": &(&1[:type] == :transforms),
        "Functions: Utilities": &(&1[:type] == :utility)
      ],
      groups_for_modules: [
        Server: [
          GRPC.Server,
          GRPC.Service,
          GRPC.Endpoint,
          GRPC.Server.Supervisor,
          GRPC.Server.Adapter,
          GRPC.Server.Adapters.Cowboy,
          GRPC.Server.Adapters.Cowboy.Handler,
          GRPC.Server.Adapters.Cowboy.Router,
          GRPC.Status,
          GRPC.Server.Stream,
          GRPC.Server.Interceptors.Logger,
          GRPC.Server.Interceptors.CORS,
          GRPC.Server.Router
        ],
        Stream: [
          GRPC.Stream,
          GRPC.Stream.Operators
        ],
        Client: [
          GRPC.Stub,
          GRPC.Channel,
          GRPC.Credential,
          GRPC.Client.Supervisor,
          GRPC.Client.Adapter,
          GRPC.Client.Adapters.Gun,
          GRPC.Client.Adapters.Mint,
          GRPC.Client.Adapters.Mint.StreamResponseProcess,
          GRPC.Client.Adapters.Mint.ConnectionProcess,
          GRPC.Client.Adapters.Mint.ConnectionProcess.State,
          GRPC.Client.Interceptor,
          GRPC.Client.Interceptors.Logger,
          GRPC.Client.Connection,
          GRPC.Client.LoadBalancing,
          GRPC.Client.LoadBalancing.RoundRobin,
          GRPC.Client.LoadBalancing.PickFirst,
          GRPC.Client.Resolver,
          GRPC.Client.Resolver.DNS,
          GRPC.Client.Resolver.DNS.Adapter,
          GRPC.Client.Resolver.Unix,
          GRPC.Client.Resolver.IPv4,
          GRPC.Client.Resolver.IPv6,
          GRPC.Client.ServiceConfig,
          GRPC.Client.Stream
        ],
        Telemetry: [
          GRPC.Telemetry
        ],
        Transport: [
          GRPC.Transport.HTTP2,
          GRPC.Transport.Utils
        ],
        Codecs: [
          GRPC.Codec,
          GRPC.Codec.Erlpack,
          GRPC.Codec.JSON,
          GRPC.Codec.Proto,
          GRPC.Codec.WebText
        ],
        Compressors: [
          GRPC.Compressor,
          GRPC.Compressor.Gzip
        ],
        "Error Handling": [
          GRPC.RPCError,
          GRPC.Server.Adapters.ReportException,
          GRPC.Logger
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

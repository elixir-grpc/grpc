defmodule GrpcClient.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-grpc/grpc_client"
  @version "1.0.0"

  def project do
    [
      app: :grpc_client,
      version: @version,
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "gRPC client implementation for Elixir",
      package: package(),
      docs: docs(),
      name: "gRPC Client",
      source_url: @source_url
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:grpc_core, path: "../grpc_core"},
      # {:grpc_core, "~> 1.0"}, # Uncomment for hex release
      {:gun, "~> 2.0"},
      {:mint, "~> 1.5"},
      {:castore, "~> 0.1 or ~> 1.0", optional: true},
      {:ex_doc, "~> 0.39", only: [:dev, :docs], runtime: false},
      {:ex_parameterized, "~> 1.3.7", only: :test},
      {:mox, "~> 1.2", only: :test},
      {:grpc_server, path: "../grpc_server", only: :test}
    ]
  end

  defp package do
    %{
      maintainers: ["Adriano Santos", "Dave Lucia", "Bing Han", "Paulo Valente"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ~w(mix.exs README.md lib src config LICENSE .formatter.exs)
    }
  end

  defp docs do
    [
      main: "GRPC.Stub",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: [
        "CHANGELOG.md",
        "guides/getting_started/client.md",
        "guides/advanced/custom_codecs.md",
        "guides/advanced/load_balancing.md"
      ],
      groups_for_modules: [
        "Client Core": [
          GRPC.Stub,
          GRPC.Channel,
          GRPC.Client.Stream,
          GRPC.Client.Supervisor,
          GRPC.Client.Connection
        ],
        Adapters: [
          GRPC.Client.Adapter,
          GRPC.Client.Adapters.Gun,
          GRPC.Client.Adapters.Mint
        ],
        "Load Balancing": [
          GRPC.Client.LoadBalancing,
          GRPC.Client.LoadBalancing.RoundRobin,
          GRPC.Client.LoadBalancing.PickFirst
        ],
        Resolvers: [
          GRPC.Client.Resolver,
          GRPC.Client.Resolver.DNS,
          GRPC.Client.Resolver.Unix,
          GRPC.Client.Resolver.IPv4,
          GRPC.Client.Resolver.IPv6
        ]
      ],
      groups_for_extras: [
        "Getting Started": ~r"^guides/getting_started/",
        Advanced: ~r"^guides/advanced/"
      ]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end

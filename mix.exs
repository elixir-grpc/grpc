defmodule GRPC.Mixfile do
  use Mix.Project

  @version "0.5.0"

  def project do
    [
      app: :grpc,
      version: @version,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      escript: escript(),
      aliases: aliases(),
      description: "The Elixir implementation of gRPC",
      docs: [
        extras: ["README.md"],
        main: "readme",
        source_ref: "v#{@version}",
        source_url: "https://github.com/elixir-grpc/grpc"
      ],
      dialyzer: [
        plt_add_deps: :apps_tree,
        plt_add_apps: [:iex, :mix, :ex_unit],
        list_unused_filters: true,
        plt_file: {:no_warn, "_build/#{Mix.env()}/plts/dialyzer.plt"}
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
      {:cowboy, "~> 2.9"},
      # This is the same as :gun 2.0.0-rc.2,
      # but we can't depend on an RC for releases
      {:gun, "~> 2.0.1", hex: :grpc_gun},
      {:jason, ">= 0.0.0", optional: true},
      {:cowlib, "~> 2.11"},
      {:protobuf, github: "elixir-protobuf/protobuf", branch: "main"},
      {:ex_doc, "~> 0.28.0", only: :dev},
      {:dialyxir, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:googleapis,
       github: "googleapis/googleapis",
       branch: "master",
       app: false,
       compile: false,
       only: [:dev, :test]}
    ]
  end

  defp package do
    %{
      maintainers: ["Bing Han"],
      licenses: ["Apache 2"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/grpc"},
      files: ~w(mix.exs README.md lib src config LICENSE .formatter.exs)
    }
  end

  defp aliases do
    [
      gen_bootstrap_protos: [&build_protobuf_escript/1, &gen_bootstrap_protos/1],
      build_protobuf_escript: &build_protobuf_escript/1
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp build_protobuf_escript(_args) do
    path = Mix.Project.deps_paths().protobuf

    File.cd!(path, fn ->
      with 0 <- Mix.shell().cmd("mix deps.get"),
           0 <- Mix.shell().cmd("mix escript.build") do
        :ok
      else
        other ->
          Mix.raise("build_protobuf_escript/1 exited with non-zero status: #{other}")
      end
    end)
  end

  # https://github.com/elixir-protobuf/protobuf/blob/cdf3acc53f619866b4921b8216d2531da52ceba7/mix.exs#L140
  defp gen_bootstrap_protos(_args) do
    proto_src = Mix.Project.deps_paths().googleapis

    protoc!("-I \"#{proto_src}\"", "./lib", [
      "google/api/http.proto",
      "google/api/annotations.proto"
    ])
  end

  defp protoc!(args, elixir_out, files_to_generate)
       when is_binary(args) and is_binary(elixir_out) and is_list(files_to_generate) do
    args =
      [
        ~s(protoc),
        ~s(--plugin=./deps/protobuf/protoc-gen-elixir),
        ~s(--elixir_out="#{elixir_out}"),
        args
      ] ++ files_to_generate

    case Mix.shell().cmd(Enum.join(args, " ")) do
      0 -> Mix.Task.rerun("format", [Path.join([elixir_out, "**", "*.pb.ex"])])
      other -> Mix.raise("'protoc' exited with non-zero status: #{other}")
    end
  end
end

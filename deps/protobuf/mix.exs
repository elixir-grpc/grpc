defmodule Protobuf.Mixfile do
  use Mix.Project

  @source_url "https://github.com/elixir-protobuf/protobuf"
  @version "0.15.0"
  @description "A pure Elixir implementation of Google Protobuf."

  def project do
    [
      app: :protobuf,
      version: @version,
      elixir: "~> 1.12",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      dialyzer: [plt_add_apps: [:mix, :jason], flags: [:no_improper_lists]],
      elixirc_paths: elixirc_paths(Mix.env()),
      test_coverage: [tool: ExCoveralls],
      test_elixirc_options: [debug_info: true],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        conformance_test: :test,
        "escript.build": :prod
      ],
      deps: deps(),
      escript: escript(),
      description: @description,
      package: package(),
      docs: docs(),
      aliases: aliases(),
      xref: [exclude: [Google.Protobuf.Any]]
    ]
  end

  def application do
    [
      extra_applications: [:logger, :eex]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "conformance", "test/support", "generated"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:jason, "~> 1.2", optional: true},

      # Dev and test dependencies
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.5", only: [:dev, :test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:stream_data, "~> 1.0", only: [:dev, :test]},
      {:excoveralls, "~> 0.14.4", only: :test},

      # We use this as a dependency because we're sneaky. It's not a Mix dependency at all,
      # it's the repo where the Protobuf source is. But this allows us to download it
      # and make sure it's there for tests without Git submodules or anything like that.
      {:google_protobuf,
       github: "protocolbuffers/protobuf",
       ref: "b407e8416e3893036aee5af9a12bd9b6a0e2b2e6",
       submodules: true,
       app: false,
       compile: false,
       only: [:dev, :test]}
    ]
  end

  defp escript do
    [main_module: Protobuf.Protoc.CLI, name: "protoc-gen-elixir"]
  end

  defp package do
    [
      maintainers: ["Bing Han", "Andrea Leopardi"],
      licenses: ["MIT"],
      links: %{"GitHub" => @source_url}
    ]
  end

  defp docs do
    [
      extras: ["README.md"],
      groups_for_modules: ["Generated Protos": ~r/^Google\.Protobuf\./],
      main: "readme",
      source_url: @source_url,
      source_ref: "v#{@version}"
    ]
  end

  defp aliases do
    [
      gen_bootstrap_protos: [&build_escript/1, &gen_bootstrap_protos/1, &gen_google_protos/1],
      gen_test_protos: [&build_escript/1, &create_generated_dir/1, &gen_test_protos/1],
      test: [
        &build_escript/1,
        &create_generated_dir/1,
        &gen_test_protos/1,
        &gen_google_test_protos/1,
        "test"
      ],
      conformance_test: [
        &build_escript/1,
        &create_generated_dir/1,
        &gen_google_test_protos/1,
        &gen_conformance_protos/1,
        fn _ -> Mix.Task.reenable("escript.build") end,
        &build_escript/1,
        &run_conformance_tests/1
      ],
      gen_bench_protos: [&build_escript/1, &gen_bench_protos/1],
      build_conformance_runner: &build_conformance_runner/1
    ]
  end

  # We need to do this in a separate shell because we want to compile "from scratch" when we do
  # things like running tests, since we usually generated some .pb.ex files on the fly and stick
  # them in a directory. That directory is in the elixirc_paths, but no files are there *before*
  # we compile the project the first time to run escript.build. It's a chicken and egg problem.
  # This way, we compile the project first to generate the escript and we do so in a subshell.
  # Then, we generate the .pb.ex files. Then we actually run stuff (like "mix test"), so that
  # compilation happens again "from scratch" and it picks up the generated files. This fixes at
  # least one problem related to extensions, because extensions are discovered by looking at
  # :application.get_key(app, :modules), and if modules are added after the first compilation and
  # loading pass, then they don't get picked up in there.
  # There is very likely a better way to do this, but this works for now.
  defp build_escript(_args) do
    # We wanna pass down MIX_ENV here because we want escript.build to happen in the same Mix
    # environment of whoever is calling this function.
    Mix.shell().cmd("mix escript.build", env: %{"MIX_ENV" => Atom.to_string(Mix.env())})
  end

  defp create_generated_dir(_args) do
    File.mkdir_p!("generated")
  end

  # These files are necessary to bootstrap the protoc-gen-elixir plugin that we generate. They
  # are committed to version control.
  defp gen_bootstrap_protos(_args) do
    proto_src = path_in_protobuf_source(["src"])

    protoc!("-I \"#{proto_src}\"", "./lib", [
      "google/protobuf/descriptor.proto",
      "google/protobuf/compiler/plugin.proto"
    ])

    protoc!("-I src -I \"#{proto_src}\"", "./lib", ["elixirpb.proto"])
  end

  defp gen_test_protos(_args) do
    proto_src = path_in_protobuf_source(["src"])

    protoc!(
      "-I #{proto_src} -I src -I test/protobuf/protoc/proto --elixir_opt=include_docs=true",
      "./generated",
      ["test/protobuf/protoc/proto/extension.proto"]
    )

    protoc!(
      "-I test/protobuf/protoc/proto --elixir_opt=package_prefix=my,include_docs=true",
      "./generated",
      ["test/protobuf/protoc/proto/test.proto", "test/protobuf/protoc/proto/service.proto"]
    )

    protoc!(
      "-I test/protobuf/protoc/proto -I #{path_in_protobuf_source("src")} " <>
        "--elixir_opt=gen_descriptors=true --elixir_opt=include_docs=true",
      "./generated",
      ["test/protobuf/protoc/proto/custom_options.proto"]
    )

    protoc!(
      "-I test/protobuf/protoc/proto --elixir_opt=include_docs=true",
      "./generated",
      ["test/protobuf/protoc/proto/no_package.proto"]
    )
  end

  defp gen_bench_protos(_args) do
    proto_bench = path_in_protobuf_source(["benchmarks"])
    protoc!("-I \"#{proto_bench}\"", "./bench/lib", benchmark_proto_files())
  end

  defp gen_google_protos(_args) do
    proto_src = path_in_protobuf_source(["src"])

    files = ~w(
      google/protobuf/any.proto
      google/protobuf/duration.proto
      google/protobuf/empty.proto
      google/protobuf/field_mask.proto
      google/protobuf/struct.proto
      google/protobuf/timestamp.proto
      google/protobuf/wrappers.proto
    )

    protoc!(
      "-I \"#{proto_src}\" --elixir_opt=gen_descriptors=true,include_docs=true",
      "./lib",
      files
    )
  end

  defp gen_google_test_protos(_args) do
    proto_root = path_in_protobuf_source(["src"])

    files = ~w(
      google/protobuf/test_messages_proto2.proto
      google/protobuf/test_messages_proto3.proto
    )

    protoc!("-I \"#{proto_root}\" --elixir_opt=include_docs=true", "./generated", files)
  end

  defp gen_conformance_protos(_args) do
    proto_src = path_in_protobuf_source(["conformance"])
    protoc!("-I \"#{proto_src}\"", "./generated", ["conformance.proto"])
  end

  defp path_in_protobuf_source(path) do
    protobuf_root = System.get_env("PROTOBUF_ROOT") || Mix.Project.deps_paths().google_protobuf
    Path.join([protobuf_root | List.wrap(path)])
  end

  defp benchmark_proto_files do
    [
      "benchmarks.proto",
      "datasets/google_message1/proto3/benchmark_message1_proto3.proto",
      "datasets/google_message1/proto2/benchmark_message1_proto2.proto",
      "datasets/google_message2/benchmark_message2.proto",
      "datasets/google_message3/benchmark_message3.proto",
      "datasets/google_message3/benchmark_message3_1.proto",
      "datasets/google_message3/benchmark_message3_2.proto",
      "datasets/google_message3/benchmark_message3_3.proto",
      "datasets/google_message3/benchmark_message3_4.proto",
      "datasets/google_message3/benchmark_message3_5.proto",
      "datasets/google_message3/benchmark_message3_6.proto",
      "datasets/google_message3/benchmark_message3_7.proto",
      "datasets/google_message3/benchmark_message3_8.proto",
      "datasets/google_message4/benchmark_message4.proto",
      "datasets/google_message4/benchmark_message4_1.proto",
      "datasets/google_message4/benchmark_message4_2.proto",
      "datasets/google_message4/benchmark_message4_3.proto"
    ]
  end

  defp protoc!(args, elixir_out, files_to_generate)
       when is_binary(args) and is_binary(elixir_out) and is_list(files_to_generate) do
    args =
      [
        ~s(protoc),
        ~s(--plugin=./protoc-gen-elixir),
        ~s(--elixir_out="#{elixir_out}"),
        args
      ] ++ files_to_generate

    cmd!(Enum.join(args, " "))

    Mix.Task.rerun("format", [Path.join([elixir_out, "**", "*.pb.ex"])])
  end

  defp run_conformance_tests(args) do
    {options, _args} = OptionParser.parse!(args, switches: [verbose: :boolean])

    runner = path_in_protobuf_source("conformance_test_runner")

    if not File.exists?(runner) do
      Mix.raise("""
      No conformance runner found at: #{runner}

      If you want to build the conformance runner locally from the Google Protobuf
      dependency of this library, run:

          mix build_conformance_runner

      If you have the protocolbuffers/protobuf repository cloned somewhere and want
      to use its conformance runner, you'll need to export the PROTOBUF_ROOT environment
      variable.
      """)
    end

    runner = Path.expand(runner)

    args = [
      "--enforce_recommended",
      "--failure_list",
      "conformance/exemptions.txt",
      "./conformance/runner.sh"
    ]

    args = if Keyword.get(options, :verbose, false), do: ["--verbose"] ++ args, else: args
    cmd!("#{runner} #{Enum.join(args, " ")}")
  end

  defp build_conformance_runner(_args) do
    File.cd!(Mix.Project.deps_paths().google_protobuf, fn ->
      cmd!("cmake . -Dprotobuf_BUILD_CONFORMANCE=ON")
      cmd!("cmake --build . --parallel 10")
    end)
  end

  defp cmd!(cmd) do
    Mix.shell().info([:cyan, "Running: ", :reset, cmd])

    case Mix.shell().cmd(cmd) do
      0 -> Mix.shell().info("")
      other -> Mix.raise("Command exited with non-zero status: #{other}")
    end
  end
end

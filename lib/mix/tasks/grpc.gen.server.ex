defmodule Mix.Tasks.Grpc.Gen.Server do
  @moduledoc """
  Generate Elixir code template for server from protobuf

  ## Examples

      mix grpc.gen.server priv/protos/helloworld.proto --out lib/

  ## Command line options

    * `--out` - Output path. Required
  """
  use Mix.Task
  import Mix.Generator

  @shortdoc "Generate Elixir code template for Server from protobuf"
  @external_resource Path.expand("./templates/grpc.gen.server/grpc_server.ex", :code.priv_dir(:grpc))
  @tmpl_path "priv/templates/grpc.gen.server/grpc_server.ex"

  def run(args) do
    {opts, [proto_path], _} = OptionParser.parse(args)
    if opts[:out] do
      generate(proto_path, opts[:out], opts)
    else
      Mix.raise "expected grpc.gen to receive the proto path and out path, " <>
        "got: #{inspect Enum.join(args, " ")}"
    end
  end

  defp generate(proto_path, out_path, opts) do
    proto = Mix.Tasks.Grpc.Gen.parse_proto(proto_path)
    assigns = [top_mod: Mix.Tasks.Grpc.Gen.top_mod(proto.package, proto_path, opts),
               proto: proto, compose_rpc: &__MODULE__.compose_rpc/1]
    create_file file_path(proto_path, out_path), grpc_gen_template(assigns)
  end

  @doc false
  def compose_rpc({name, request, _, req_stream, rep_stream, _}) do
    name = name |> to_string |> Macro.underscore
    request = to_var(request)
    request = if req_stream, do: request <> "_enum", else: request
    stream = if rep_stream, do: "stream", else: "_stream"
    "#{name}(#{request}, #{stream})"
  end

  defp file_path(proto_path, out_path) do
    name = Path.basename(proto_path, ".proto")
    File.mkdir_p(out_path)
    Path.join(out_path, name <> "_server.ex")
  end

  defp grpc_gen_template(binding) do
    tmpl_path = Application.app_dir(:grpc, @tmpl_path)
    EEx.eval_file(tmpl_path, binding, trim: true)
  end

  defp to_var(struct) do
    struct
    |> to_string
    |> String.split(".")
    |> List.last
    |> Macro.underscore
  end
end

defmodule Mix.Tasks.Grpc.Gen do
  @external_resource Path.expand("../../priv/templates/grpc.gen.ex", __DIR__)
  use Mix.Task

  import Macro, only: [camelize: 1]
  import Mix.Generator

  alias GRPC.Proto

  @shortdoc "Generate Elixir code for Service and Stub from protobuf"
  @tmpl_path "priv/templates/grpc.gen.ex"

  @moduledoc """
  Generates Elixir code from protobuf

  ## Examples

      mix grpc.gen priv/protos/helloworld.proto --out lib/

  Protobuf file should be in the same project because generated Elixir file
  depends on it. Check the generated file for detail.

  The top level module name will be generated from package name by default,
  but you can custom it with `--namespace` option.

  ## Command line options

    * `--namespace` - Custom top level module name

  """

  def run(args) do
    case OptionParser.parse(args) do
      {[out: out_path], [proto_path], _} ->
        generate(proto_path, out_path)
      {_, _, _} ->
        Mix.raise "expected grpc.gen to receive the proto path and out path, " <>
          "got: #{inspect Enum.join(args, " ")}"
    end
  end

  def generate(proto_path, out_path) do
    proto = parse_proto(proto_path)
    assigns = [top_mod: top_mod(proto.package), proto_path: proto_path(proto_path, out_path), proto: proto,
               service_prefix: service_prefix(proto.package) ]
    create_file file_path(proto_path, out_path), grpc_gen_template(assigns)
  end

  def parse_proto(proto_path) do
    parsed = Protobuf.Parser.parse_files!([proto_path])
    proto = %Proto{}
    services = []
    proto = Enum.reduce parsed, proto, fn(item, proto) ->
      case item do
        {:package, package} ->
          %{proto | package: to_string(package)}
        {{:service, service_name}, rpcs} ->
          rpcs = Enum.map(rpcs, fn(rpc) -> Tuple.delete_at(rpc, 0) end)
          service_name = service_name |> to_string |> camelize
          service = %Proto.Service{name: service_name , rpcs: rpcs}
          %{proto | services: [service|services]}
        _ -> proto
      end
    end
    package = proto.package || Path.basename(proto_path, ".proto")
    %{proto | services: Enum.reverse(proto.services), package: package}
  end

  def top_mod(package) do
    package
    |> to_string
    |> String.split(".")
    |> Enum.map(fn(seg)-> camelize(seg) end)
    |> Enum.join(".")
  end

  def service_prefix(package)  do
    if package && String.length(package) > 0, do: package <> ".", else: ""
  end

  def proto_path(proto_path, out_path) do
    proto_path = Path.relative_to_cwd(proto_path)
    level = out_path |> Path.relative_to_cwd |> Path.split |> length
    prefix = List.duplicate("..", level) |> Enum.join("/")
    Path.join(prefix, proto_path)
  end

  def file_path(proto_path, out_path) do
    name = Path.basename(proto_path, ".proto")
    File.mkdir_p(out_path)
    Path.join(out_path, name <> ".pb.ex")
  end

  def grpc_gen_template(binding) do
    tmpl_path = Application.app_dir(:grpc, @tmpl_path)
    EEx.eval_file(tmpl_path, binding, trim: true)
  end
end

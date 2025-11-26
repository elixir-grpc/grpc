defmodule Protobuf.Protoc.CLI do
  @moduledoc """
  `protoc` plugin for generating Elixir code.

  `protoc-gen-elixir` (this name is important) **must** be in `$PATH`. You are not supposed
  to call it directly, but only through `protoc`.

  ## Examples

      $ protoc --elixir_out=./lib your.proto
      $ protoc --elixir_out=plugins=grpc:./lib/ *.proto
      $ protoc -I protos --elixir_out=./lib protos/namespace/*.proto

  Options:

    * --version       Print version of protobuf-elixir
    * --help (-h)     Print this help

  """

  alias Protobuf.Protoc.Context

  # Entrypoint for the escript (protoc-gen-elixir).
  @doc false
  @spec main([String.t()]) :: :ok
  def main(args)

  def main(["--version"]) do
    {:ok, version} = :application.get_key(:protobuf, :vsn)
    IO.puts(version)
  end

  def main([opt]) when opt in ["--help", "-h"] do
    IO.puts(@moduledoc)
  end

  # When called through protoc, all input is passed through stdin.
  def main([] = _args) do
    Protobuf.load_extensions()

    # See https://groups.google.com/forum/#!topic/elixir-lang-talk/T5enez_BBTI.
    :io.setopts(:standard_io, encoding: :latin1)

    # Read the standard input that protoc feeds us.
    bin = binread_all!(:stdio)

    request = Protobuf.Decoder.decode(bin, Google.Protobuf.Compiler.CodeGeneratorRequest)

    ctx =
      %Context{}
      |> parse_params(request.parameter || "")
      |> find_types(request.proto_file, request.file_to_generate)

    {files, package_level_extensions} =
      Enum.flat_map_reduce(request.file_to_generate, %{}, fn file, acc ->
        desc = Enum.find(request.proto_file, &(&1.name == file))
        {package_level_extensions, files} = Protobuf.Protoc.Generator.generate(ctx, desc)

        acc =
          case package_level_extensions do
            {mod_name, extensions} -> Map.update(acc, mod_name, extensions, &(&1 ++ extensions))
            nil -> acc
          end

        {files, acc}
      end)

    ext_files =
      for {mod_name, extensions} <- package_level_extensions do
        {mod_name, contents} =
          Protobuf.Protoc.Generator.Extension.generate_package_level(ctx, mod_name, extensions)

        %Google.Protobuf.Compiler.CodeGeneratorResponse.File{
          name: Macro.underscore(mod_name) <> ".pb.ex",
          content: contents
        }
      end

    %Google.Protobuf.Compiler.CodeGeneratorResponse{
      file: files ++ ext_files,
      supported_features: supported_features()
    }
    |> Protobuf.encode_to_iodata()
    |> IO.binwrite()
  end

  def main(_args) do
    raise "invalid arguments. See protoc-gen-elixir --help."
  end

  def supported_features() do
    # The only available feature is proto3 with optional fields.
    # This is backwards compatible with proto2 optional fields.
    Google.Protobuf.Compiler.CodeGeneratorResponse.Feature.value(:FEATURE_PROTO3_OPTIONAL)
  end

  # Made public for testing.
  @doc false
  def parse_params(%Context{} = ctx, params_str) when is_binary(params_str) do
    params_str
    |> String.split(",")
    |> Enum.reduce(ctx, &parse_param/2)
  end

  defp parse_param("plugins=" <> plugins, ctx) do
    %Context{ctx | plugins: String.split(plugins, "+")}
  end

  defp parse_param("gen_descriptors=" <> value, ctx) do
    case value do
      "true" ->
        %Context{ctx | gen_descriptors?: true}

      other ->
        raise "invalid value for gen_descriptors option, expected \"true\", got: #{inspect(other)}"
    end
  end

  defp parse_param("package_prefix=" <> package, ctx) do
    if package == "" do
      raise "package_prefix can't be empty"
    else
      %Context{ctx | package_prefix: package}
    end
  end

  defp parse_param("transform_module=" <> module, ctx) do
    %Context{ctx | transform_module: Module.concat([module])}
  end

  defp parse_param("one_file_per_module=" <> value, ctx) do
    case value do
      "true" ->
        %Context{ctx | one_file_per_module?: true}

      other ->
        raise "invalid value for one_file_per_module option, expected \"true\", got: #{inspect(other)}"
    end
  end

  defp parse_param("include_docs=" <> value, ctx) do
    case value do
      "true" ->
        %Context{ctx | include_docs?: true}

      other ->
        raise "invalid value for include_docs option, expected \"true\", got: #{inspect(other)}"
    end
  end

  defp parse_param(_unknown, ctx) do
    ctx
  end

  # Made public for testing.
  @doc false
  @spec find_types(Context.t(), [Google.Protobuf.FileDescriptorProto.t()], [String.t()]) ::
          Context.t()
  def find_types(%Context{} = ctx, descs, files_to_generate)
      when is_list(descs) and is_list(files_to_generate) do
    global_type_mapping =
      Map.new(descs, fn %Google.Protobuf.FileDescriptorProto{name: filename} = desc ->
        {filename, find_types_in_proto(ctx, desc, files_to_generate)}
      end)

    %Context{ctx | global_type_mapping: global_type_mapping}
  end

  defp find_types_in_proto(
         %Context{} = ctx,
         %Google.Protobuf.FileDescriptorProto{} = desc,
         files_to_generate
       ) do
    # Only take package_prefix into consideration for files that we're directly generating.
    package_prefix =
      if desc.name in files_to_generate do
        ctx.package_prefix
      else
        nil
      end

    ctx =
      %Protobuf.Protoc.Context{
        namespace: [],
        package_prefix: package_prefix,
        package: desc.package
      }
      |> Protobuf.Protoc.Context.custom_file_options_from_file_desc(desc)

    find_types_in_descriptor(_types = %{}, ctx, desc.message_type ++ desc.enum_type)
  end

  defp find_types_in_descriptor(types_acc, ctx, descs) when is_list(descs) do
    Enum.reduce(descs, types_acc, &find_types_in_descriptor(_acc = &2, ctx, _desc = &1))
  end

  defp find_types_in_descriptor(
         types_acc,
         ctx,
         %Google.Protobuf.DescriptorProto{name: name} = desc
       ) do
    new_ctx = update_in(ctx.namespace, &(&1 ++ [name]))

    types_acc
    |> update_types(ctx, name)
    |> find_types_in_descriptor(new_ctx, desc.enum_type)
    |> find_types_in_descriptor(new_ctx, desc.nested_type)
  end

  defp find_types_in_descriptor(
         types_acc,
         ctx,
         %Google.Protobuf.EnumDescriptorProto{name: name}
       ) do
    update_types(types_acc, ctx, name)
  end

  defp update_types(types, %Context{namespace: ns, package: pkg} = ctx, name) do
    type_name = Protobuf.Protoc.Generator.Util.mod_name(ctx, ns ++ [name])

    mapping_name =
      ([pkg] ++ ns ++ [name])
      |> Enum.reject(&is_nil/1)
      |> Enum.join(".")

    Map.put(types, "." <> mapping_name, %{type_name: type_name})
  end

  if Version.match?(System.version(), "~> 1.13") do
    defp binread_all!(device) do
      case IO.binread(device, :eof) do
        data when is_binary(data) -> data
        :eof -> _previous_behavior = ""
        other -> raise "reading from #{inspect(device)} failed: #{inspect(other)}"
      end
    end
  else
    defp binread_all!(device) do
      case IO.binread(device, :all) do
        data when is_binary(data) -> data
        other -> raise "reading from #{inspect(device)} failed: #{inspect(other)}"
      end
    end
  end
end

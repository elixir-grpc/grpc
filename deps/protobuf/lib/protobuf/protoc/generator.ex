defmodule Protobuf.Protoc.Generator do
  @moduledoc false

  alias Protobuf.Protoc.Context
  alias Protobuf.Protoc.Generator

  # TODO: improve spec
  @spec generate(Context.t(), %Google.Protobuf.FileDescriptorProto{}) ::
          {term(), [Google.Protobuf.Compiler.CodeGeneratorResponse.File.t()]}
  def generate(%Context{} = ctx, %Google.Protobuf.FileDescriptorProto{} = desc) do
    {package_level_extensions, module_definitions} = generate_module_definitions(ctx, desc)

    files =
      if ctx.one_file_per_module? do
        Enum.map(module_definitions, fn {mod_name, content} ->
          file_name = Macro.underscore(mod_name) <> ".pb.ex"
          %Google.Protobuf.Compiler.CodeGeneratorResponse.File{name: file_name, content: content}
        end)
      else
        # desc.name is the filename, ending in ".proto".
        file_name = Path.rootname(desc.name) <> ".pb.ex"

        content =
          module_definitions
          |> Enum.map(fn {_mod_name, contents} -> [contents, ?\n] end)
          |> IO.iodata_to_binary()
          |> Generator.Util.format()

        [%Google.Protobuf.Compiler.CodeGeneratorResponse.File{name: file_name, content: content}]
      end

    {package_level_extensions, files}
  end

  defp generate_module_definitions(ctx, %Google.Protobuf.FileDescriptorProto{} = desc) do
    ctx =
      %Context{
        ctx
        | comments: Protobuf.Protoc.Generator.Comment.parse(desc),
          syntax: syntax(desc.syntax),
          package: desc.package,
          dep_type_mapping: get_dep_type_mapping(ctx, desc.dependency, desc.name)
      }
      |> Protobuf.Protoc.Context.custom_file_options_from_file_desc(desc)

    enum_defmodules =
      desc.enum_type
      |> Enum.with_index()
      |> Enum.map(fn {enum, index} ->
        {Context.append_comment_path(ctx, "5.#{index}"), enum}
      end)
      |> Enum.map(fn {ctx, enum} -> Generator.Enum.generate(ctx, enum) end)

    {nested_enum_defmodules, message_defmodules} =
      Generator.Message.generate_list(ctx, desc.message_type)

    {package_level_extensions, extension_defmodules} = Generator.Extension.generate(ctx, desc)

    service_defmodules =
      if "grpc" in ctx.plugins do
        desc.service
        |> Enum.with_index()
        |> Enum.map(fn {service, index} ->
          Generator.Service.generate(
            Context.append_comment_path(ctx, "6.#{index}"),
            service
          )
        end)
      else
        []
      end

    defmodules =
      List.flatten([
        enum_defmodules,
        nested_enum_defmodules,
        message_defmodules,
        service_defmodules,
        extension_defmodules
      ])

    {package_level_extensions, defmodules}
  end

  defp get_dep_type_mapping(%Context{global_type_mapping: global_mapping}, deps, file_name) do
    mapping =
      Enum.reduce(deps, %{}, fn dep, acc ->
        Map.merge(acc, global_mapping[dep])
      end)

    Map.merge(mapping, global_mapping[file_name])
  end

  defp syntax("proto3"), do: :proto3
  defp syntax("proto2"), do: :proto2
  defp syntax(nil), do: :proto2
end

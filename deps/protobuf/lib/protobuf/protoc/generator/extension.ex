defmodule Protobuf.Protoc.Generator.Extension do
  @moduledoc false

  alias Google.Protobuf.{DescriptorProto, FieldDescriptorProto, FileDescriptorProto}
  alias Protobuf.Protoc.Context
  alias Protobuf.Protoc.Generator.Comment
  alias Protobuf.Protoc.Generator.Util

  require EEx

  @ext_postfix "PbExtension"

  EEx.function_from_file(
    :defp,
    :extension_template,
    Path.expand("./templates/extension.ex.eex", :code.priv_dir(:protobuf)),
    [:assigns]
  )

  # Returns a tuple of {module_name, module_contents} with all the given extensions.
  @spec generate_package_level(Context.t(), String.t(), [String.t()]) ::
          {module_name :: String.t(), contents :: String.t()}
  def generate_package_level(%Context{} = ctx, mod_name, extensions)
      when is_binary(mod_name) and is_list(extensions) do
    use_options =
      Util.options_to_str(%{
        syntax: ctx.syntax,
        protoc_gen_elixir_version: "\"#{Util.version()}\""
      })

    module_contents =
      Util.format(
        extension_template(
          comment: Comment.get(ctx),
          use_options: use_options,
          module: mod_name,
          extends: extensions,
          module_doc?: ctx.include_docs?
        )
      )

    {mod_name, module_contents}
  end

  @spec generate(Context.t(), FileDescriptorProto.t()) :: {package_level_extensions, modules}
        when package_level_extensions:
               {module_name :: String.t(), extension_dsls :: [String.t()]} | nil,
             modules: [{module_name :: String.t(), contents :: String.t()}]
  def generate(%Context{} = ctx, %FileDescriptorProto{} = file_desc) do
    use_options =
      Util.options_to_str(%{
        syntax: ctx.syntax,
        protoc_gen_elixir_version: "\"#{Util.version()}\""
      })

    nested_modules = get_extensions_from_messages(ctx, use_options, file_desc.message_type)

    {package_level_extensions(ctx, file_desc), nested_modules}
  end

  defp package_level_extensions(%Context{}, %FileDescriptorProto{extension: []}) do
    nil
  end

  defp package_level_extensions(%Context{} = ctx, %FileDescriptorProto{extension: extensions}) do
    namespace = Util.mod_name(ctx, ctx.namespace ++ [Macro.camelize(@ext_postfix)])
    {namespace, Enum.map(extensions, &generate_extend_dsl(ctx, &1, _ns = ""))}
  end

  defp generate_extend_dsl(ctx, %FieldDescriptorProto{} = f, ns) do
    extendee = Util.type_from_type_name(ctx, f.extendee)
    f = Protobuf.Protoc.Generator.Message.get_field(ctx, f)

    name =
      if ns == "" do
        f.name
      else
        inspect("#{ns}.#{f.name}")
      end

    "#{extendee}, :#{name}, #{f.number}, #{f.label}: true, type: #{f.type}#{f.opts_str}"
  end

  defp get_extensions_from_messages(%Context{} = ctx, use_options, descs) do
    descs
    |> Enum.with_index()
    |> Enum.flat_map(fn {desc, index} ->
      generate_module(Context.append_comment_path(ctx, "7.#{index}"), use_options, desc) ++
        get_extensions_from_messages(
          %Context{
            Context.append_comment_path(ctx, "6.#{index}")
            | namespace: ctx.namespace ++ [Macro.camelize(desc.name)]
          },
          use_options,
          desc.nested_type
        )
    end)
  end

  defp generate_module(%Context{}, _use_options, %DescriptorProto{extension: []}) do
    []
  end

  defp generate_module(%Context{} = ctx, use_options, %DescriptorProto{} = desc) do
    ns = ctx.namespace ++ [Macro.camelize(desc.name)]
    module_name = Util.mod_name(ctx, ns ++ [Macro.camelize(@ext_postfix)])

    module_contents =
      Util.format(
        extension_template(
          comment: Comment.get(ctx),
          module: module_name,
          use_options: use_options,
          extends: Enum.map(desc.extension, &generate_extend_dsl(ctx, &1, _ns = "")),
          module_doc?: ctx.include_docs?
        )
      )

    [{module_name, module_contents}]
  end
end

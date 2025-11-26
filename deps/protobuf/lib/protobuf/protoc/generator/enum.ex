defmodule Protobuf.Protoc.Generator.Enum do
  @moduledoc false

  alias Protobuf.Protoc.Context
  alias Protobuf.Protoc.Generator.Comment
  alias Protobuf.Protoc.Generator.Util

  require EEx

  EEx.function_from_file(
    :defp,
    :enum_template,
    Path.expand("./templates/enum.ex.eex", :code.priv_dir(:protobuf)),
    [:assigns]
  )

  @spec generate(Context.t(), Google.Protobuf.EnumDescriptorProto.t()) ::
          {module_name :: String.t(), file_contents :: String.t()}
  def generate(%Context{namespace: ns} = ctx, %Google.Protobuf.EnumDescriptorProto{} = desc) do
    msg_name = Util.mod_name(ctx, ns ++ [Macro.camelize(desc.name)])

    use_options =
      Util.options_to_str(%{
        syntax: ctx.syntax,
        enum: true,
        protoc_gen_elixir_version: "\"#{Util.version()}\""
      })

    descriptor_fun_body =
      if ctx.gen_descriptors? do
        Util.descriptor_fun_body(desc)
      else
        nil
      end

    content =
      enum_template(
        comment: Comment.get(ctx),
        module: msg_name,
        use_options: use_options,
        fields: desc.value,
        descriptor_fun_body: descriptor_fun_body,
        module_doc?: ctx.include_docs?
      )

    {msg_name, Util.format(content)}
  end
end

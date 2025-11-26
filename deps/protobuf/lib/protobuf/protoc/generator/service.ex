defmodule Protobuf.Protoc.Generator.Service do
  @moduledoc false

  alias Protobuf.Protoc.Context
  alias Protobuf.Protoc.Generator.Comment
  alias Protobuf.Protoc.Generator.Util

  require EEx

  EEx.function_from_file(
    :defp,
    :service_template,
    Path.expand("./templates/service.ex.eex", :code.priv_dir(:protobuf)),
    [:assigns]
  )

  @spec generate(Context.t(), Google.Protobuf.ServiceDescriptorProto.t()) ::
          {String.t(), String.t()}
  def generate(%Context{} = ctx, %Google.Protobuf.ServiceDescriptorProto{} = desc) do
    # service can't be nested
    mod_name = Util.mod_name(ctx, [Macro.camelize(desc.name)])
    name = Util.prepend_package_prefix(ctx.package, desc.name)
    methods = Enum.map(desc.method, &generate_service_method(ctx, &1))

    descriptor_fun_body =
      if ctx.gen_descriptors? do
        Util.descriptor_fun_body(desc)
      else
        nil
      end

    {mod_name,
     Util.format(
       service_template(
         comment: Comment.get(ctx),
         module: mod_name,
         service_name: name,
         package: ctx.package,
         methods: methods,
         descriptor_fun_body: descriptor_fun_body,
         version: Util.version(),
         module_doc?: ctx.include_docs?
       )
     )}
  end

  defp generate_service_method(ctx, method) do
    input = service_arg(Util.type_from_type_name(ctx, method.input_type), method.client_streaming)

    output =
      service_arg(Util.type_from_type_name(ctx, method.output_type), method.server_streaming)

    {method.name, input, output}
  end

  defp service_arg(type, _streaming? = true), do: "stream(#{type})"
  defp service_arg(type, _streaming?), do: type
end

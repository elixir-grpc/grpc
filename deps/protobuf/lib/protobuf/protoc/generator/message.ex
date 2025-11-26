defmodule Protobuf.Protoc.Generator.Message do
  @moduledoc false

  alias Google.Protobuf.{DescriptorProto, FieldDescriptorProto}

  alias Protobuf.Protoc.Context
  alias Protobuf.Protoc.Generator.Comment
  alias Protobuf.Protoc.Generator.Util
  alias Protobuf.Protoc.Generator.Enum, as: EnumGenerator

  require EEx

  EEx.function_from_file(
    :defp,
    :message_template,
    Path.expand("./templates/message.ex.eex", :code.priv_dir(:protobuf)),
    [:assigns]
  )

  @spec generate_list(Context.t(), [Google.Protobuf.DescriptorProto.t()]) ::
          {enums :: [{mod_name :: String.t(), contents :: String.t()}],
           messages :: [{mod_name :: String.t(), contents :: String.t()}]}
  def generate_list(%Context{} = ctx, descs) when is_list(descs) do
    descs
    |> Enum.with_index()
    |> Enum.map(fn {desc, index} ->
      generate(Context.append_comment_path(ctx, "4.#{index}"), desc)
    end)
    |> Enum.unzip()
  end

  @spec generate(Context.t(), Google.Protobuf.DescriptorProto.t()) :: {any(), any()}
  def generate(%Context{} = ctx, %Google.Protobuf.DescriptorProto{} = desc) do
    new_ns = ctx.namespace ++ [Macro.camelize(desc.name)]
    msg_name = Util.mod_name(ctx, new_ns)
    fields = get_fields(ctx, desc)
    extensions = get_extensions(desc)

    descriptor_fun_body =
      if ctx.gen_descriptors? do
        Util.descriptor_fun_body(desc)
      else
        nil
      end

    ctx = %Context{ctx | namespace: new_ns}
    {nested_enums, nested_msgs} = Enum.unzip(gen_nested_msgs(ctx, desc))

    msg =
      {msg_name,
       Util.format(
         message_template(
           comment: Comment.get(ctx),
           module: msg_name,
           use_options: msg_opts_str(ctx, desc.options),
           oneofs: desc.oneof_decl,
           fields: gen_fields(ctx.syntax, fields),
           descriptor_fun_body: descriptor_fun_body,
           transform_module: ctx.transform_module,
           extensions: extensions,
           module_doc?: ctx.include_docs?
         )
       )}

    {gen_nested_enums(ctx, desc) ++ nested_enums, nested_msgs ++ [msg]}
  end

  defp gen_nested_msgs(ctx, desc) do
    desc.nested_type
    |> Enum.with_index()
    |> Enum.map(fn {msg_desc, index} ->
      generate(Context.append_comment_path(ctx, "3.#{index}"), msg_desc)
    end)
  end

  defp gen_nested_enums(ctx, desc) do
    desc.enum_type
    |> Enum.with_index()
    |> Enum.map(fn {enum_desc, index} ->
      EnumGenerator.generate(Context.append_comment_path(ctx, "4.#{index}"), enum_desc)
    end)
  end

  defp gen_fields(syntax, fields) do
    Enum.map(fields, fn %{opts_str: opts_str} = f ->
      field(syntax, f, opts_str)
    end)
  end

  defp field(:proto3, %{proto3_optional: true, label: "optional"} = f, opts_str) do
    ":#{f[:name]}, #{f[:number]}, proto3_optional: true, type: #{f[:type]}#{opts_str}"
  end

  defp field(syntax, f, opts_str) do
    label_str =
      if syntax == :proto3 && f[:label] != "repeated", do: "", else: "#{f[:label]}: true, "

    ":#{f[:name]}, #{f[:number]}, #{label_str}type: #{f[:type]}#{opts_str}"
  end

  defp msg_opts_str(%{syntax: syntax}, opts) do
    msg_options = opts

    opts = %{
      syntax: syntax,
      map: msg_options && msg_options.map_entry,
      deprecated: msg_options && msg_options.deprecated,
      protoc_gen_elixir_version: "\"#{Util.version()}\""
    }

    str = Util.options_to_str(opts)
    if String.length(str) > 0, do: ", " <> str, else: ""
  end

  defp get_fields(ctx, desc) do
    oneofs = get_real_oneofs(desc.oneof_decl)

    nested_maps = nested_maps(ctx, desc)

    for {field, index} <- Enum.with_index(desc.field) do
      get_field(
        Context.append_comment_path(ctx, "2.#{index}"),
        field,
        nested_maps,
        oneofs
      )
    end
  end

  # Public and used by extensions.
  @spec get_field(Context.t(), FieldDescriptorProto.t()) :: map()
  def get_field(%Context{} = ctx, %FieldDescriptorProto{} = field) do
    get_field(ctx, field, _nested_maps = %{}, _oneofs = [])
  end

  defp get_field(ctx, %FieldDescriptorProto{} = field_desc, nested_maps, oneofs) do
    opts = field_options(field_desc, ctx.syntax)

    # Check if the field is a map.
    map = nested_maps[field_desc.type_name]
    opts = if map, do: Keyword.put(opts, :map, true), else: opts

    opts =
      cond do
        field_desc.oneof_index == nil -> opts
        oneofs == [] or field_desc.proto3_optional -> opts
        true -> Keyword.put(opts, :oneof, field_desc.oneof_index)
      end

    opts_str =
      opts
      |> sort_field_opts_to_reduce_changes()
      |> Enum.map_join(", ", fn {key, val} -> "#{key}: #{inspect(val)}" end)

    opts_str = if opts_str == "", do: "", else: ", " <> opts_str

    type = field_type_name(ctx, field_desc)

    %{
      name: field_desc.name,
      comment: Comment.get(ctx),
      number: field_desc.number,
      label: label_name(field_desc.label),
      type: type,
      type_enum: field_desc.type,
      opts: Map.new(opts),
      opts_str: opts_str,
      map: map,
      oneof: field_desc.oneof_index,
      proto3_optional: field_desc.proto3_optional || false
    }
  end

  defp get_real_oneofs(oneof_decl) do
    Enum.flat_map(oneof_decl, fn oneof ->
      if String.starts_with?(oneof.name, "_"), do: [], else: [oneof.name]
    end)
  end

  # To avoid unnecessarily changing the files that users of this library generated with previous
  # versions, we try to guarantee an order of field options in the generated files.
  ordered_opts = [
    :json_name,
    :optional,
    :repeated,
    :map,
    :type,
    :default,
    :enum,
    :oneof,
    :packed,
    :deprecated
  ]

  weights = Map.new(Enum.with_index(ordered_opts))

  defp sort_field_opts_to_reduce_changes(opts) do
    Enum.sort_by(opts, fn {key, _val} -> Map.fetch!(unquote(Macro.escape(weights)), key) end)
  end

  defp get_extensions(desc) do
    max = Protobuf.Extension.max()

    Enum.map(desc.extension_range, fn %DescriptorProto.ExtensionRange{start: start, end: end_} ->
      if end_ == max do
        {Integer.to_string(start), "Protobuf.Extension.max()"}
      else
        {Integer.to_string(start), Integer.to_string(end_)}
      end
    end)
  end

  defp field_type_name(ctx, %FieldDescriptorProto{type_name: type_name} = field_desc) do
    case from_enum(field_desc.type) do
      type when type in [:enum, :message] and not is_nil(type_name) ->
        Util.type_from_type_name(ctx, type_name)

      type ->
        inspect(type)
    end
  end

  # Map of protobuf are actually nested(one level) messages
  defp nested_maps(ctx, desc) do
    fully_qualified_name =
      ([ctx.package | ctx.namespace] ++ [desc.name])
      |> Enum.reject(&is_nil/1)
      |> Enum.join(".")

    prefix = "." <> fully_qualified_name

    Enum.reduce(desc.nested_type, %{}, fn desc, acc ->
      if desc.options && desc.options.map_entry do
        [k, v] = Enum.sort(desc.field, &(&1.number < &2.number))

        pair = {{k.type, field_type_name(ctx, k)}, {v.type, field_type_name(ctx, v)}}

        Map.put(acc, "#{prefix}.#{desc.name}", pair)
      else
        acc
      end
    end)
  end

  defp field_options(field_desc, syntax) do
    (_starting_opts = [])
    |> add_default_value_to_opts(field_desc)
    |> add_enum_to_opts(field_desc)
    |> add_json_name_to_opts(syntax, field_desc)
    |> add_field_opts_if_present(field_desc)
  end

  defp add_field_opts_if_present(opts, %FieldDescriptorProto{options: field_opts}) do
    opts =
      if field_opts && is_boolean(field_opts.packed) do
        Keyword.put(opts, :packed, field_opts.packed)
      else
        opts
      end

    opts =
      if field_opts && is_boolean(field_opts.deprecated) do
        Keyword.put(opts, :deprecated, field_opts.deprecated)
      else
        opts
      end

    opts
  end

  defp add_enum_to_opts(opts, %FieldDescriptorProto{type: type}) do
    if type == :TYPE_ENUM do
      Keyword.put(opts, :enum, true)
    else
      opts
    end
  end

  defp label_name(:LABEL_OPTIONAL), do: "optional"
  defp label_name(:LABEL_REQUIRED), do: "required"
  defp label_name(:LABEL_REPEATED), do: "repeated"

  defp add_default_value_to_opts(opts, %FieldDescriptorProto{
         default_value: default_value
       })
       when default_value in [nil, ""] do
    opts
  end

  defp add_default_value_to_opts(opts, %FieldDescriptorProto{
         default_value: default_value,
         type: type
       }) do
    value = cast_default_value(type, default_value)

    if is_nil(value) do
      opts
    else
      Keyword.put(opts, :default, value)
    end
  end

  int_types = [
    :TYPE_INT64,
    :TYPE_UINT64,
    :TYPE_INT32,
    :TYPE_FIXED64,
    :TYPE_FIXED32,
    :TYPE_UINT32,
    :TYPE_SFIXED32,
    :TYPE_SFIXED64,
    :TYPE_SINT32,
    :TYPE_SINT64
  ]

  float_types = [:TYPE_DOUBLE, :TYPE_FLOAT]

  defp cast_default_value(:TYPE_BOOL, "true"), do: true
  defp cast_default_value(:TYPE_BOOL, "false"), do: false
  defp cast_default_value(:TYPE_ENUM, val), do: String.to_atom(val)
  defp cast_default_value(type, val) when type in [:TYPE_STRING, :TYPE_BYTES], do: val
  defp cast_default_value(type, val) when type in unquote(int_types), do: int_default(val)
  defp cast_default_value(type, val) when type in unquote(float_types), do: float_default(val)

  defp float_default(value) do
    # A float can also be "inf", "NaN", and so on.
    case Float.parse(value) do
      {float, ""} -> float
      :error -> value
      {_float, _rest} -> raise "unparseable float/double default: #{inspect(value)}"
    end
  end

  defp int_default(value) do
    case Integer.parse(value) do
      {int, ""} -> int
      _other -> raise "unparseable number default: #{inspect(value)}"
    end
  end

  # Omit `json_name` from the options list when it matches the original field
  # name to keep the list small.
  defp add_json_name_to_opts(opts, _, %{name: name, json_name: name}), do: opts

  defp add_json_name_to_opts(opts, _, %{json_name: json_name}),
    do: Keyword.put(opts, :json_name, json_name)

  defp add_json_name_to_opts(opts, _syntax, _props), do: opts

  defp from_enum(:TYPE_DOUBLE), do: :double
  defp from_enum(:TYPE_FLOAT), do: :float
  defp from_enum(:TYPE_INT64), do: :int64
  defp from_enum(:TYPE_UINT64), do: :uint64
  defp from_enum(:TYPE_INT32), do: :int32
  defp from_enum(:TYPE_FIXED64), do: :fixed64
  defp from_enum(:TYPE_FIXED32), do: :fixed32
  defp from_enum(:TYPE_BOOL), do: :bool
  defp from_enum(:TYPE_STRING), do: :string
  defp from_enum(:TYPE_GROUP), do: :group
  defp from_enum(:TYPE_MESSAGE), do: :message
  defp from_enum(:TYPE_BYTES), do: :bytes
  defp from_enum(:TYPE_UINT32), do: :uint32
  defp from_enum(:TYPE_ENUM), do: :enum
  defp from_enum(:TYPE_SFIXED32), do: :sfixed32
  defp from_enum(:TYPE_SFIXED64), do: :sfixed64
  defp from_enum(:TYPE_SINT32), do: :sint32
  defp from_enum(:TYPE_SINT64), do: :sint64
end

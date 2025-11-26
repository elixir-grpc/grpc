defmodule Protobuf.DSL do
  @doc """
  Define a field for the message.

  Corresponds to Protobuf declarations such as:

      string query = 1;

  or more generally

      <type> <name> = <field_number>;

  ## Arguments

    * `name` — must be an atom representing the name of the field.
    * `field_number` — must be an integer representing the field number.
    * `options` - a keyword list of options, see below.

  ## Options

    * `:proto3_optional` - boolean representing whether the field is optional with the `proto3`
      syntax. See [the
      documentation](https://developers.google.com/protocol-buffers/docs/proto3#oneof)

    * `:type` - atom representing the type of the field.

    * `:repeated` - boolean representing whether the field is repeated.

    * `:optional` - boolean representing whether the field is optional.

    * `:required` - boolean representing whether the field is required.

    * `:enum` - boolean representing whether the field is a possible value of an enum.

    * `:map` - boolean representing whether the field is a part of a map.

    * `:default` - the default value of the field. Must be a valid Elixir term at compile time,
      that can be encoded with Protobuf and matches with the type of the field.

    * `:packed` - boolean representing whether the field is packed.

    * `:deprecated` - boolean representing whether the field is deprecated.

    * `:json_name` - if present, specifies the name of the field when using the JSON mapping (see
      `Protobuf.JSON`). If not present, the default mapping will be used.

  """
  defmacro field(name, field_number, options \\ []) do
    quote bind_quoted: [name: name, fnum: field_number, options: options] do
      if not is_atom(name) do
        raise ArgumentError, "expected atom as the field name, got: #{inspect(name)}"
      end

      if not is_integer(fnum) do
        raise ArgumentError,
              "expected integer as the field number, got: #{inspect(fnum)}"
      end

      if not Keyword.keyword?(options) do
        raise ArgumentError, "expected a keyword list as the options, got: #{inspect(options)}"
      end

      @fields {name, fnum, options}
    end
  end

  @doc """
  Define oneof in the message module.
  """
  defmacro oneof(name, index) do
    quote do
      @oneofs {unquote(name), unquote(index)}
    end
  end

  @doc """
  Define "extend" for a message(the first argument module).
  """
  defmacro extend(mod, name, fnum, options) do
    quote do
      @extends {unquote(mod), unquote(name), unquote(fnum), unquote(options)}
    end
  end

  @doc """
  Define extensions range in the message module to allow extensions for this module.

  Extension ranges are defined as a list of tuples `{start, end}`, where each tuple is
  an **exclusive** range starting and `start` and ending at `end` (the equivalent
  of `start..end-1` in Elixir).

  To simulate the Protobuf `max` keyword, you can use `Protobuf.Extension.max/0`.

  ## Examples

  These Protobuf definition:

  ```protobuf
  message Foo {
    extensions 1, 10 to 20, 100 to max;
  }
  ```

  Would be translated in Elixir to:

      extensions [{1, 2}, {10, 21}, {100, Protobuf.Extension.max()}]

  """
  defmacro extensions(ranges) do
    quote do
      ranges = unquote(ranges)

      if not is_list(ranges) do
        raise ArgumentError, "expected a list of ranges, got: #{inspect(ranges)}"
      end

      Enum.each(ranges, fn
        value when not is_tuple(value) or tuple_size(value) != 2 ->
          raise ArgumentError, "expected a range, got: #{inspect(value)}"

        {left, right} when not is_integer(left) or not is_integer(right) ->
          raise ArgumentError, "expected an integer range, got: #{inspect({left, right})}"

        {left, right} when left >= right ->
          raise ArgumentError, "expected an ordered range, got: #{inspect({left, right})}"

        other ->
          :ok
      end)

      @extensions unquote(ranges)
    end
  end

  alias Protobuf.FieldProps
  alias Protobuf.MessageProps
  alias Protobuf.Wire

  # Registered as the @on_definition compile callback for modules that call "use Protobuf"
  # Allow us to detect when `transform_module` is re-defined
  def on_def(_env, :def, :transform_module, [], [], do: nil) do
    :ok
  end

  def on_def(env, :def, :transform_module, [], [], do: module_alias_ast) do
    Module.put_attribute(env.module, :transform_module, module_alias_ast)
    :ok
  end

  def on_def(_, _, _, _, _, _) do
    :ok
  end

  # Registered as the @before_compile callback for modules that call "use Protobuf".
  defmacro __before_compile__(env) do
    fields = Module.get_attribute(env.module, :fields)
    options = Module.get_attribute(env.module, :options)
    oneofs = Module.get_attribute(env.module, :oneofs)
    extensions = Module.get_attribute(env.module, :extensions)

    extension_props =
      Module.get_attribute(env.module, :extends)
      |> gen_extension_props()

    msg_props = generate_message_props(fields, oneofs, extensions, options)

    defines_t_type? = Module.defines_type?(env.module, {:t, 0})
    defines_defstruct? = Module.defines?(env.module, {:__struct__, 1})
    transform_module_ast = Module.get_attribute(env.module, :transform_module)

    quote do
      @spec __message_props__() :: Protobuf.MessageProps.t()
      def __message_props__ do
        unquote(Macro.escape(msg_props))
      end

      if unquote(defines_defstruct?) or unquote(defines_t_type?) do
        raise Protobuf.InvalidError, """
        since v0.10.0 of the :protobuf library, the t/0 type and the struct are automatically \
        generated for modules that call "use Protobuf" if they are Protobuf enums or messages, \
        and their usage was deprecated. Since v0.15.0, they are errors. Remove your explicit \
        definition of both of these or regenerate the files with the latest version of the \
        protoc-gen-elixir plugin.\
        """
      else
        unquote(def_t_typespec(msg_props, extension_props, transform_module_ast))
        unquote(gen_defstruct(msg_props))
      end

      unquote(msg_props.enum? && Protobuf.DSL.Enum.quoted_enum_functions(msg_props))

      if unquote(Macro.escape(extension_props)) != nil do
        def __protobuf_info__(:extension_props) do
          unquote(Macro.escape(extension_props))
        end
      end

      if unquote(Macro.escape(extensions)) do
        unquote(def_extension_functions())
      end
    end
  end

  defp def_t_typespec(props, extension_props, transform_module_ast)
       when not is_nil(transform_module_ast) do
    default_typespec = def_t_typespec(props, extension_props, nil)

    quote do
      require unquote(transform_module_ast)

      if macro_exported?(unquote(transform_module_ast), :typespec, 1) do
        unquote(transform_module_ast).typespec(unquote(default_typespec))
      else
        unquote(default_typespec)
      end
    end
  end

  defp def_t_typespec(%MessageProps{enum?: true} = props, _extension_props, _) do
    quote do
      @type t() :: unquote(Protobuf.DSL.Typespecs.quoted_enum_typespec(props))
    end
  end

  defp def_t_typespec(%MessageProps{} = props, _extension_props = nil, _) do
    quote do
      @type t() :: unquote(Protobuf.DSL.Typespecs.quoted_message_typespec(props))
    end
  end

  defp def_t_typespec(_props, _extension_props, _) do
    nil
  end

  defp def_extension_functions() do
    quote do
      def put_extension(%{} = map, extension_mod, field, value) do
        Protobuf.Extension.put(__MODULE__, map, extension_mod, field, value)
      end

      def get_extension(struct, extension_mod, field, default \\ nil) do
        Protobuf.Extension.get(struct, extension_mod, field, default)
      end
    end
  end

  defp generate_message_props(fields, oneofs, extensions, options) do
    syntax = Keyword.get(options, :syntax, :proto2)

    field_props =
      Map.new(fields, fn {name, fnum, opts} -> {fnum, field_props(syntax, name, fnum, opts)} end)

    # The "reverse" of field props, that is, a map from atom name to field number.
    # We calculate this from "fields" instead of just reversing "field_props" because
    # enum fields can have aliases, that is, names that have the same integer tag. "field_props"
    # is a map with field tags as keys, so fields with the same "field_tags" have been
    # squashed together.
    field_tags = Map.new(fields, fn {name, fnum, _opts} -> {name, fnum} end)

    repeated_fields =
      for {_fnum, %FieldProps{repeated?: true, name_atom: name}} <- field_props,
          do: name

    embedded_fields =
      for {_fnum, %FieldProps{embedded?: true, map?: false, name_atom: name}} <- field_props,
          do: name

    %MessageProps{
      tags_map: Map.new(fields, fn {_, fnum, _} -> {fnum, fnum} end),
      ordered_tags: field_props |> Map.keys() |> Enum.sort(),
      field_props: field_props,
      field_tags: field_tags,
      repeated_fields: repeated_fields,
      embedded_fields: embedded_fields,
      syntax: syntax,
      oneof: Enum.reverse(oneofs),
      enum?: Keyword.get(options, :enum) == true,
      map?: Keyword.get(options, :map) == true,
      extension_range: extensions
    }
  end

  defp gen_extension_props([_ | _] = extends) do
    extensions =
      Map.new(extends, fn {extendee, name_atom, fnum, opts} ->
        # Only proto2 has extensions
        props = field_props(:proto2, name_atom, fnum, opts)

        props = %Protobuf.Extension.Props.Extension{
          extendee: extendee,
          field_props: props
        }

        {{extendee, fnum}, props}
      end)

    name_to_tag =
      Map.new(extends, fn {extendee, name_atom, fnum, _opts} ->
        {{extendee, name_atom}, {extendee, fnum}}
      end)

    %Protobuf.Extension.Props{extensions: extensions, name_to_tag: name_to_tag}
  end

  defp gen_extension_props(_) do
    nil
  end

  defp field_props(syntax, name, fnum, opts) do
    %FieldProps{
      fnum: fnum,
      name: Atom.to_string(name),
      name_atom: name
    }
    |> parse_field_opts_to_field_props(opts)
    |> verify_no_default_in_proto3(syntax)
    |> wrap_enum_type()
    |> cal_label(syntax)
    |> cal_json_name()
    |> cal_embedded()
    |> cal_packed(syntax)
    |> cal_repeated()
    |> cal_encoded_fnum()
  end

  defp parse_field_opts_to_field_props(%FieldProps{} = props, opts) do
    Enum.reduce(opts, props, fn
      {:optional, optional?}, acc ->
        %FieldProps{acc | optional?: optional?}

      {:proto3_optional, proto3_optional?}, acc ->
        %FieldProps{acc | proto3_optional?: proto3_optional?}

      {:required, required?}, acc ->
        %FieldProps{acc | required?: required?}

      {:enum, enum?}, acc ->
        %FieldProps{acc | enum?: enum?}

      {:map, map?}, acc ->
        %FieldProps{acc | map?: map?}

      {:repeated, repeated?}, acc ->
        %FieldProps{acc | repeated?: repeated?}

      {:embedded, embedded}, acc ->
        %FieldProps{acc | embedded?: embedded}

      {:deprecated, deprecated?}, acc ->
        %FieldProps{acc | deprecated?: deprecated?}

      {:packed, packed?}, acc ->
        %FieldProps{acc | packed?: packed?}

      {:type, type}, acc ->
        %FieldProps{acc | type: type}

      {:default, default}, acc ->
        %FieldProps{acc | default: default}

      {:oneof, oneof}, acc ->
        %FieldProps{acc | oneof: oneof}

      {:json_name, json_name}, acc ->
        %FieldProps{acc | json_name: json_name}
    end)
  end

  defp cal_label(%FieldProps{} = props, :proto3) do
    if props.required? do
      raise Protobuf.InvalidError, message: "required can't be used in proto3"
    else
      %FieldProps{props | optional?: true}
    end
  end

  defp cal_label(props, _syntax), do: props

  defp wrap_enum_type(%FieldProps{enum?: true, type: type} = props) do
    %FieldProps{props | type: {:enum, type}, wire_type: Wire.wire_type({:enum, type})}
  end

  defp wrap_enum_type(%FieldProps{type: type} = props) do
    %FieldProps{props | wire_type: Wire.wire_type(type)}
  end

  # The compiler always emits a json name, but we omit it in the DSL when it
  # matches the name, to keep it uncluttered. Now we infer it back from name.
  defp cal_json_name(%FieldProps{json_name: name} = props) when is_binary(name), do: props
  defp cal_json_name(props), do: %FieldProps{props | json_name: props.name}

  defp verify_no_default_in_proto3(%FieldProps{} = props, syntax) do
    if syntax == :proto3 and not is_nil(props.default) do
      raise Protobuf.InvalidError, message: "default can't be used in proto3"
    else
      props
    end
  end

  defp cal_embedded(%FieldProps{type: type, enum?: false} = props) when is_atom(type) do
    case to_string(type) do
      "Elixir." <> _ -> %FieldProps{props | embedded?: true}
      _ -> props
    end
  end

  defp cal_embedded(props), do: props

  defp cal_packed(%FieldProps{packed?: true, repeated?: repeated?} = props, _syntax) do
    cond do
      props.embedded? -> raise ":packed can't be used with :embedded field"
      repeated? -> %FieldProps{props | packed?: true}
      true -> raise ":packed must be used with :repeated"
    end
  end

  defp cal_packed(%FieldProps{packed?: false} = props, _syntax) do
    props
  end

  defp cal_packed(%FieldProps{type: type, repeated?: true} = props, :proto3) do
    packed? = (props.enum? or not props.embedded?) and type_numeric?(type)
    %FieldProps{props | packed?: packed?}
  end

  defp cal_packed(props, _syntax), do: %FieldProps{props | packed?: false}

  defp cal_repeated(%FieldProps{map?: true} = props), do: %FieldProps{props | repeated?: false}

  defp cal_repeated(%FieldProps{repeated?: true, oneof: oneof}) when not is_nil(oneof),
    do: raise(":oneof can't be used with repeated")

  defp cal_repeated(props), do: props

  defp cal_encoded_fnum(%FieldProps{fnum: fnum, packed?: true} = props) do
    encoded_fnum = Protobuf.Encoder.encode_fnum(fnum, Wire.wire_type(:bytes))
    %FieldProps{props | encoded_fnum: encoded_fnum}
  end

  defp cal_encoded_fnum(%FieldProps{fnum: fnum, wire_type: wire_type} = props) do
    encoded_fnum = Protobuf.Encoder.encode_fnum(fnum, wire_type)
    %FieldProps{props | encoded_fnum: encoded_fnum}
  end

  defp gen_defstruct(%MessageProps{} = message_props) do
    regular_fields =
      for {_fnum, %FieldProps{oneof: nil} = prop} <- message_props.field_props,
          do: {prop.name_atom, field_default(message_props.syntax, prop)}

    oneof_fields =
      for {name_atom, _fnum} <- message_props.oneof,
          do: {name_atom, _struct_default = nil}

    extension_fields =
      if message_props.extension_range do
        [{:__pb_extensions__, _default = %{}}]
      else
        []
      end

    unknown_fields = {:__unknown_fields__, _default = []}

    struct_fields = regular_fields ++ oneof_fields ++ extension_fields ++ [unknown_fields]

    quote do
      defstruct unquote(Macro.escape(struct_fields))
    end
  end

  defp type_numeric?(:int32), do: true
  defp type_numeric?(:int64), do: true
  defp type_numeric?(:uint32), do: true
  defp type_numeric?(:uint64), do: true
  defp type_numeric?(:sint32), do: true
  defp type_numeric?(:sint64), do: true
  defp type_numeric?(:bool), do: true
  defp type_numeric?({:enum, _}), do: true
  defp type_numeric?(:fixed32), do: true
  defp type_numeric?(:sfixed32), do: true
  defp type_numeric?(:fixed64), do: true
  defp type_numeric?(:sfixed64), do: true
  defp type_numeric?(:float), do: true
  defp type_numeric?(:double), do: true
  defp type_numeric?(_), do: false

  # Used by Protobuf.Decoder
  @doc false
  def field_default(syntax, field_props)

  def field_default(_syntax, %FieldProps{default: default}) when not is_nil(default), do: default
  def field_default(_syntax, %FieldProps{repeated?: true}), do: []
  def field_default(_syntax, %FieldProps{map?: true}), do: %{}
  def field_default(:proto3, %FieldProps{proto3_optional?: true}), do: nil
  def field_default(:proto3, props), do: type_default(props.type)
  def field_default(_syntax, _props), do: nil

  defp type_default(:int32), do: 0
  defp type_default(:int64), do: 0
  defp type_default(:uint32), do: 0
  defp type_default(:uint64), do: 0
  defp type_default(:sint32), do: 0
  defp type_default(:sint64), do: 0
  defp type_default(:bool), do: false
  defp type_default({:enum, mod}), do: mod.key(0)
  defp type_default(:fixed32), do: 0
  defp type_default(:sfixed32), do: 0
  defp type_default(:fixed64), do: 0
  defp type_default(:sfixed64), do: 0
  defp type_default(:float), do: 0.0
  defp type_default(:double), do: 0.0
  defp type_default(:bytes), do: <<>>
  defp type_default(:string), do: ""
  defp type_default(:message), do: nil
  defp type_default(:group), do: nil
  defp type_default(_), do: nil
end

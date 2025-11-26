defmodule Protobuf.DSL.Typespecs do
  @moduledoc false

  alias Protobuf.{FieldProps, MessageProps}

  @spec quoted_enum_typespec(MessageProps.t()) :: Macro.t()
  def quoted_enum_typespec(%MessageProps{field_props: field_props}) do
    atom_specs =
      field_props
      |> Enum.sort_by(fn {fnum, _prop} -> fnum end)
      |> Enum.map(fn {_fnum, %FieldProps{name_atom: name}} -> name end)
      |> union_specs()

    quote do
      integer() | unquote(atom_specs)
    end
  end

  @spec quoted_message_typespec(MessageProps.t()) :: Macro.t()
  def quoted_message_typespec(%MessageProps{syntax: syntax} = message_props) do
    regular_fields =
      for {_fnum, %FieldProps{oneof: nil} = prop} <- message_props.field_props,
          do: {prop.name_atom, field_prop_to_spec(syntax, prop)}

    oneof_fields =
      for {field_name, fnum} <- message_props.oneof do
        possible_fields =
          for {_fnum, %FieldProps{oneof: ^fnum} = prop} <- message_props.field_props, do: prop

        {field_name, oneof_spec(syntax, possible_fields)}
      end

    extension_fields =
      case message_props.extension_range do
        [_ | _] -> [{:__pb_extensions__, quote(do: map())}]
        _other -> []
      end

    unknown_fields = [
      {:__unknown_fields__,
       quote(
         do: [
           Protobuf.unknown_field()
         ]
       )}
    ]

    field_specs = regular_fields ++ oneof_fields ++ extension_fields ++ unknown_fields

    quote do: %__MODULE__{unquote_splicing(field_specs)}
  end

  defp oneof_spec(syntax, possible_oneof_fields) do
    possible_oneof_fields
    |> Enum.map(fn prop -> {prop.name_atom, field_prop_to_spec(syntax, prop)} end)
    |> Kernel.++([nil])
    |> union_specs()
  end

  defp field_prop_to_spec(_syntax, %FieldProps{map?: true, type: map_mod} = prop) do
    Code.ensure_compiled!(map_mod)
    map_props = map_mod.__message_props__()

    key_spec = scalar_type_to_spec(map_props.field_props[map_props.field_tags.key].type)
    value_prop = map_props.field_props[map_props.field_tags.value]

    value_spec = type_to_spec(value_prop.type, value_prop)

    value_spec = if prop.embedded?, do: quote(do: unquote(value_spec) | nil), else: value_spec
    quote do: %{optional(unquote(key_spec)) => unquote(value_spec)}
  end

  defp field_prop_to_spec(syntax, %FieldProps{type: type} = prop) do
    spec = type_to_spec(type, prop)

    cond do
      prop.repeated? ->
        quote do: [unquote(spec)]

      prop.embedded? or (prop.optional? and is_nil(prop.oneof) and syntax != :proto3) or
          prop.proto3_optional? ->
        quote do: unquote(spec) | nil

      true ->
        spec
    end
  end

  defp type_to_spec({:enum, enum_mod}, _prop), do: quote(do: unquote(enum_mod).t())
  defp type_to_spec(mod, %FieldProps{embedded?: true}), do: quote(do: unquote(mod).t())
  defp type_to_spec(:group, _prop), do: quote(do: term())
  defp type_to_spec(type, _prop), do: scalar_type_to_spec(type)

  defp scalar_type_to_spec(:string), do: quote(do: String.t())
  defp scalar_type_to_spec(:bytes), do: quote(do: binary())
  defp scalar_type_to_spec(:bool), do: quote(do: boolean())

  defp scalar_type_to_spec(type)
       when type in [:int32, :int64, :sint32, :sint64, :sfixed32, :sfixed64],
       do: quote(do: integer())

  defp scalar_type_to_spec(type)
       when type in [:uint32, :uint64, :fixed32, :fixed64],
       do: quote(do: non_neg_integer())

  defp scalar_type_to_spec(type) when type in [:float, :double],
    do: quote(do: float() | :infinity | :negative_infinity | :nan)

  # We do this because the :| operator is left-associative, so if we just map and build "acc |
  # spec" then we end up with "((foo | bar) | baz) | bong". By building it from right to left, it
  # works just fine.
  defp union_specs(specs) do
    Enum.reduce(Enum.reverse(specs), fn spec, acc ->
      quote do: unquote(spec) | unquote(acc)
    end)
  end
end

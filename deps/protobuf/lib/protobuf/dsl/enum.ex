defmodule Protobuf.DSL.Enum do
  @moduledoc false

  alias Protobuf.{FieldProps, MessageProps}

  @callback value(atom()) :: integer()
  @callback value(tag) :: tag when tag: integer()

  @callback key(integer()) :: atom() | integer()

  @callback mapping() :: %{optional(atom()) => tag} when tag: integer()

  @callback __reverse_mapping__() :: %{optional(tag) => atom()} when tag: integer()

  @spec quoted_enum_functions(MessageProps.t()) :: Macro.t()
  def quoted_enum_functions(%MessageProps{enum?: true} = message_props) do
    if message_props.syntax == :proto3 and not Map.has_key?(message_props.field_props, 0) do
      first_enum_tag = message_props.field_props |> Map.keys() |> Enum.min()
      raise "the first enum value must have tag 0 in proto3, got: #{first_enum_tag}"
    end

    [quote(do: @behaviour(unquote(__MODULE__)))] ++
      defp_value_callback(message_props) ++
      defp_key_callback(message_props) ++
      defp_mapping_callbacks(message_props)
  end

  defp defp_value_callback(message_props) do
    bodiless_clause =
      quote do
        @impl unquote(__MODULE__)
        def value(atom)
      end

    atom_clauses =
      for {atom, tag} <- message_props.field_tags do
        quote do
          def value(unquote(atom)), do: unquote(tag)
        end
      end

    int_clause =
      quote do
        def value(tag) when is_integer(tag) and tag >= 0, do: tag
      end

    [bodiless_clause] ++ atom_clauses ++ [int_clause]
  end

  defp defp_key_callback(message_props) do
    bodiless_clause =
      quote do
        @impl unquote(__MODULE__)
        def key(atom)
      end

    int_clauses =
      for {tag, %FieldProps{name_atom: atom}} <- message_props.field_props do
        quote do
          def key(unquote(tag)), do: unquote(atom)
        end
      end

    catchall_clause =
      quote do
        def key(tag) when is_integer(tag) and tag >= 0, do: tag
      end

    [bodiless_clause] ++ int_clauses ++ [catchall_clause]
  end

  defp defp_mapping_callbacks(message_props) do
    reverse_mapping =
      for {atom, tag} <- message_props.field_tags,
          key <- [tag, Atom.to_string(atom)],
          into: %{},
          do: {key, atom}

    [
      quote do
        @impl unquote(__MODULE__)
        def mapping(), do: unquote(Macro.escape(message_props.field_tags))
      end
    ] ++
      [
        quote do
          @impl unquote(__MODULE__)
          def __reverse_mapping__(), do: unquote(Macro.escape(reverse_mapping))
        end
      ]
  end
end

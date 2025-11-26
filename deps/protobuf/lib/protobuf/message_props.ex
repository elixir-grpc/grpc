defmodule Protobuf.MessageProps do
  @moduledoc false

  alias Protobuf.FieldProps

  # A struct containing information about a Protobuf message

  # A "field number".
  @type tag() :: integer()

  @type field_name() :: atom()

  @type t :: %__MODULE__{
          ordered_tags: [tag()],
          tags_map: %{tag() => tag()},
          field_props: %{tag() => FieldProps.t()},
          field_tags: %{field_name() => tag()},
          repeated_fields: [field_name()],
          embedded_fields: [field_name()],
          syntax: atom(),
          oneof: [{field_name(), tag()}],
          enum?: boolean(),
          extendable?: boolean(),
          map?: boolean(),

          # See Protobuf.DSL.extensions/1.
          extension_range: [{non_neg_integer(), non_neg_integer()}] | nil
        }

  defstruct ordered_tags: [],
            tags_map: %{},
            field_props: %{},
            field_tags: %{},
            repeated_fields: [],
            embedded_fields: [],
            syntax: :proto2,
            oneof: [],
            enum?: false,
            extendable?: false,
            map?: false,
            extension_range: []
end

defmodule Protobuf.FieldProps do
  @moduledoc false

  # A struct containing information about a field in a Protobuf message.

  @type t :: %__MODULE__{
          fnum: integer,
          name: String.t(),
          name_atom: atom,
          json_name: String.t(),
          wire_type: Protobuf.wire_type(),
          type: atom | {:enum, atom},
          default: any,
          oneof: non_neg_integer | nil,
          required?: boolean,
          optional?: boolean,
          proto3_optional?: boolean,
          repeated?: boolean,
          enum?: boolean,
          embedded?: boolean,
          packed?: boolean,
          map?: boolean,
          deprecated?: boolean,
          encoded_fnum: iodata
        }
  defstruct fnum: nil,
            name: nil,
            name_atom: nil,
            json_name: nil,
            wire_type: nil,
            type: nil,
            default: nil,
            oneof: nil,
            required?: false,
            optional?: false,
            proto3_optional?: false,
            repeated?: false,
            enum?: false,
            embedded?: false,
            packed?: nil,
            map?: false,
            deprecated?: false,
            encoded_fnum: nil
end

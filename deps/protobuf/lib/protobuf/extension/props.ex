defmodule Protobuf.Extension.Props do
  @moduledoc false

  defmodule Extension do
    @moduledoc false
    @type t :: %__MODULE__{
            extendee: module,
            field_props: FieldProps.T
          }
    defstruct extendee: nil,
              field_props: nil
  end

  @type t :: %__MODULE__{
          extensions: %{{module, integer} => Extension.t()},
          name_to_tag: %{{module, atom} => {module, integer}}
        }
  defstruct extensions: %{}, name_to_tag: %{}
end

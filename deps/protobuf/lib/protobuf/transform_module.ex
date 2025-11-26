defmodule Protobuf.TransformModule do
  @moduledoc """
  Behaviour for transformer modules.

  By defining a `transform_module/0` function on your protobuf message module
  you can add custom encoding and decoding logic for your message.

  As an example we can use this to implement a message that will be decoded as a string value:

      defmodule StringMessage do
        use Protobuf, syntax: :proto3

        field :value, 1, type: :string

        def transform_module(), do: MyTransformModule
      end

  The transformer behaviour implementation:

      defmodule MyTransformModule do
        @behaviour Protobuf.TransformModule

        defmacro typespec(_default_ast) do
          quote do
            @type t() :: String.t()
          end
        end

        @impl true
        def encode(string, StringMessage) when is_binary(string), do: struct(StringMessage, value: string)

        @impl true
        def decode(%{value: string}, StringMessage), do: string
      end

  Notice that since the `c:typespec/1` macro was introduced, transform modules can't
  depend on the types that they transform anymore in compile time, meaning struct
  syntax can't be used.
  """

  @type value() :: term()
  @type type() :: module()
  @type message() :: struct()

  @doc """
  Takes any Elixir term and the protobuf message type and encodes it into
  that type.

  Called before a message is encoded.
  """
  @callback encode(value(), type()) :: message()

  @doc """
  Takes any protobuf message and the message type and encodes it into arbitrary
  Elixir term.

  Called after a message is decoded.
  """
  @callback decode(message(), type()) :: value()

  @doc """
  Transforms the typespec for modules using this transformer.

  If this callback is not present, the default typespec will be used.
  """
  @macrocallback typespec(default_typespec :: Macro.t()) :: Macro.t()

  @optional_callbacks [typespec: 1]
end

defmodule Google.Type.Decimal do
  @moduledoc """
  A representation of a decimal value, such as 2.5. Clients may convert values
  into language-native decimal formats, such as Java's [BigDecimal][] or
  Python's [decimal.Decimal][].

  [BigDecimal]:
  https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/math/BigDecimal.html
  [decimal.Decimal]: https://docs.python.org/3/library/decimal.html
  """

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field :value, 1, type: :string
end

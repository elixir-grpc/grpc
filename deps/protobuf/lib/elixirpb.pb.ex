defmodule Elixirpb.FileOptions do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto2

  field :module_prefix, 1, optional: true, type: :string, json_name: "modulePrefix"
end

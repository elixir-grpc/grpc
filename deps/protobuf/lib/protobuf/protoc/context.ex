defmodule Protobuf.Protoc.Context do
  @moduledoc false

  @type t() :: %__MODULE__{}

  # Plugins passed by options
  defstruct plugins: [],

            ### All files scope

            # All parsed comments from the source file (mapping from path to comment)
            # %{"1.4.2" => "this is a comment", "1.5.2.4.2" => "more comment\ndetails"}
            comments: %{},

            # Mapping from file name to (mapping from type name to metadata, like elixir type name)
            # %{"example.proto" => %{".example.FooMsg" => %{type_name: "Example.FooMsg"}}}
            global_type_mapping: %{},

            ### One file scope

            # Package name
            package: nil,
            package_prefix: nil,
            module_prefix: nil,
            syntax: nil,
            # Mapping from type_name to metadata. It's merged type mapping of dependencies files including itself
            # %{".example.FooMsg" => %{type_name: "Example.FooMsg"}}
            dep_type_mapping: %{},

            # For a message
            # Nested namespace when generating nested messages. It should be joined to get the full namespace
            namespace: [],

            # Include binary descriptors in the generated protobuf modules
            # And expose them via the `descriptor/0` function
            gen_descriptors?: false,

            # Module to transform values before and after encode and decode
            transform_module: nil,

            # Generate one file per module with "proper" directory structure
            # (according to Elixir conventions) if this is true
            one_file_per_module?: false,

            # Include visible module docs in the generated protobuf modules
            include_docs?: false,

            # Elixirpb.FileOptions
            custom_file_options: %{},

            # Current comment path. The locations are encoded into with a joining "."
            # character. E.g. "4.2.3.0"
            current_comment_path: ""

  @spec custom_file_options_from_file_desc(t(), Google.Protobuf.FileDescriptorProto.t()) :: t()
  def custom_file_options_from_file_desc(ctx, desc)

  def custom_file_options_from_file_desc(
        %__MODULE__{} = ctx,
        %Google.Protobuf.FileDescriptorProto{options: nil}
      ) do
    %__MODULE__{ctx | custom_file_options: %{}}
  end

  def custom_file_options_from_file_desc(
        %__MODULE__{} = ctx,
        %Google.Protobuf.FileDescriptorProto{options: options}
      ) do
    custom_file_opts =
      Google.Protobuf.FileOptions.get_extension(options, Elixirpb.PbExtension, :file) ||
        %Elixirpb.FileOptions{}

    %__MODULE__{
      ctx
      | custom_file_options: custom_file_opts,
        module_prefix: Map.get(custom_file_opts, :module_prefix)
    }
  end

  @doc """
  Appends a comment path to the current comment path.

  ## Examples

      iex> append_comment_path(%{current_comment_path: "4"}, "2.4")
      %{current_comment_path: "4.2.4"}

  """
  def append_comment_path(ctx, path) do
    %{ctx | current_comment_path: String.trim(ctx.current_comment_path <> "." <> path, ".")}
  end
end

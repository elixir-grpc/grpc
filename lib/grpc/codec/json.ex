if Code.ensure_loaded?(Jason) do
  defmodule GRPC.Codec.JSON do
    @moduledoc """
    JSON Codec for gRPC communication.

    This module implements the `GRPC.Codec` behaviour, providing encoding and decoding functions
    for JSON serialization in the context of gRPC communication.

    ## Configuration

    By default, the codec uses `emit_unpopulated: true` to include all fields with default values.
    You can override this behavior by:

    1. Passing options to `encode/2`:
       ```elixir
       GRPC.Codec.JSON.encode(struct, emit_unpopulated: false)
       ```

    2. Setting application config:
       ```elixir
       config :grpc, GRPC.Codec.JSON, emit_unpopulated: false
       ```

    This module requires the Jason dependency.
    """
    @behaviour GRPC.Codec

    def name(), do: "json"

    @doc """
    Encodes a struct using the Protobuf.JSON.encode!/2 function.

    By default, uses `emit_unpopulated: true` unless configured otherwise via application config
    or explicitly passed in opts.

    ### Parameters:

    - `struct` - The struct to be encoded.
    - `opts` - Keyword list of options to pass to Protobuf.JSON.encode!/2. Defaults to `[]`.

    ### Returns:

    The encoded binary data.

    ### Examples:

    ```elixir
    # Using default options (from config or emit_unpopulated: true)
    %MyStruct{id: 1, name: "John"} |> GRPC.Codec.JSON.encode()

    # Overriding with custom options
    %MyStruct{id: 1, name: "John"} |> GRPC.Codec.JSON.encode(emit_unpopulated: false)
    ```

    """
    def encode(struct, opts \\ []) do
      opts = Keyword.merge(get_default_encode_opts(), opts)
      Protobuf.JSON.encode!(struct, opts)
    end

    defp get_default_encode_opts do
      Application.get_env(:grpc, __MODULE__, emit_unpopulated: true)
    end

    @doc """
    Decodes binary data into a map using the Jason library.
    Parameters:

        binary - The binary data to be decoded.
        module - Module to be created.

    Returns:

    A map representing the decoded data.

    Raises:

    Raises an error if the Jason library is not loaded.

    Example:

    ```elixir
    binary_data |> GRPC.Codec.JSON.decode(__MODULE__)
    ```
    """
    def decode(<<>>, _module), do: %{}

    def decode(binary, _module), do: Jason.decode!(binary)
  end
else
  defmodule GRPC.Codec.JSON do
    def decode(_, _), do: raise(ArgumentError, "Module Jason not found")
  end
end

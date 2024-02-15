defmodule GRPC.Codec.JSON do
  @moduledoc """
  JSON Codec for gRPC communication.

  This module implements the `GRPC.Codec` behaviour, providing encoding and decoding functions
  for JSON serialization in the context of gRPC communication.

  ## Behavior Functions

  - `name/0`: Returns the name of the codec, which is "json".
  - `encode/1`: Encodes a struct using the Protobuf.JSON.encode!/1 function.
  - `decode/2`: Decodes binary data into a map using the Jason library.

  This module requires the Jason dependency.

  """
  @behaviour GRPC.Codec

  def name() do
    "json"
  end

  @doc """
  Encodes a struct using the Protobuf.JSON.encode!/1 function.

  ### Parameters:

  - `struct` - The struct to be encoded.

  ### Returns:

  The encoded binary data.

  ### Example:

  ```elixir
  %MyStruct{id: 1, name: "John"} |> GRPC.Codec.JSON.encode()
  ```

  """
  def encode(struct) do
    Protobuf.JSON.encode!(struct)
  end

  @doc """
  Decodes binary data into a map using the Jason library.
  Parameters:

      binary - The binary data to be decoded.
      module - Module to be created.

  Returns:

  A map representing the decoded data.

  Raises:

  Raises an error if the :jason library is not loaded.

  Example:

  ```elixir
  binary_data |> GRPC.Codec.JSON.decode(__MODULE__)
  ```
  """
  def decode(<<>>, _module) do
    %{}
  end

  def decode(binary, _module) do
    if jason = load_jason() do
      jason.decode!(binary)
    else
      raise "`:jason` library not loaded"
    end
  end

  defp load_jason, do: Code.ensure_loaded?(Jason) and Jason
end

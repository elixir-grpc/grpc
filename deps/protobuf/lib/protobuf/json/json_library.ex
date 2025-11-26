defmodule Protobuf.JSON.JSONLibrary do
  @moduledoc false
  # Uses `JSON` for Elixir >= 1.18, Jason if Elixir < 1.18 and Jason available,
  # or returns error otherwise

  cond do
    Code.ensure_loaded?(JSON) ->
      def encode_to_iodata(encodable) do
        try do
          {:ok, JSON.encode_to_iodata!(encodable)}
        rescue
          exception ->
            {:error, exception}
        end
      end

      def decode(data) do
        case JSON.decode(data) do
          {:ok, decoded} -> {:ok, decoded}
          {:error, error} -> {:error, Protobuf.JSON.DecodeError.new(error)}
        end
      end

    Code.ensure_loaded?(Jason) ->
      def encode_to_iodata(encodable), do: Jason.encode_to_iodata(encodable)
      def decode(data), do: Jason.decode(data)

    true ->
      def encode_to_iodata(_), do: {:error, EncodeError.new(:no_json_lib)}
      def decode(_), do: {:error, EncodeError.new(:no_json_lib)}
  end
end

defmodule GRPC.Server.Router.Query do
  @moduledoc false
  # This module is based on https://github.com/elixir-plug/plug/blob/main/lib/plug/conn/query.ex
  # Decoding of URL-encoded queries as per the rules outlined in the documentation for [`google.api.HttpRule`](https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule)
  # It provides similar functionality to `URI.decode_query/3` or `Plug.Conn.Query.decode/4` with the following differences:

  # 1. A repeated key is treated as a list of values
  # 1. Sub-paths on the form `path.subpath` are decoded as nested maps
  # 1. Sub-paths with the same leaf key are decoded as a list

  alias GRPC.Server.Router.FieldPath

  @spec decode(String.t(), map()) :: %{optional(String.t()) => term()}
  def decode(query, acc \\ %{})

  def decode("", acc) do
    acc
  end

  def decode(query, acc) when is_binary(query) do
    parts = :binary.split(query, "&", [:global])

    Enum.reduce(
      Enum.reverse(parts),
      acc,
      &decode_www_pair(&1, &2)
    )
  end

  defp decode_www_pair("", acc) do
    acc
  end

  defp decode_www_pair(binary, acc) do
    current =
      case :binary.split(binary, "=") do
        [key, value] ->
          {decode_www_form(key), decode_www_form(value)}

        [key] ->
          {decode_www_form(key), ""}
      end

    FieldPath.decode_pair(current, acc)
  end

  defp decode_www_form(value) do
    URI.decode_www_form(value)
  end
end

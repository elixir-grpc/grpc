defmodule GRPC.Server.Transcode.Query do
  # This module is based on https://github.com/elixir-plug/plug/blob/main/lib/plug/conn/query.ex

  @moduledoc """
  Decoding of URL-encoded queries as per the rules outlined in the documentation for [`google.api.HttpRule`](https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule)

  It provides similar functionality to `URI.decode_query/3` or `Plug.Conn.Query.decode/4` with the following differences:

  1. A repeated key is treated as a list of values
  1. Sub-paths on the form `path.subpath` are decoded as nested maps
  1. Sub-paths with the same leaf key are decoded as a list
  """

  @doc """
  Decodes the given `query`.

  The `query` is assumed to be encoded in the "x-www-form-urlencoded" format.

  `acc` is the initial "accumulator" where decoded values will be added.

  ## Examples

      iex> decode("a=A&b=B")
      %{"a" => "A", "b" => "B"}

      iex> decode("param=A&param=B")
      %{"param" => ["A", "B"]}

      iex> decode("root.a=A&root.b=B")
      %{"root" => %{"a" => "A", "b" => "B"}}

      iex> decode("root.a=A&root.a=B")
      %{"root" => ["A", "B"]}

  """
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

    decode_pair(current, acc)
  end

  defp decode_www_form(value) do
    URI.decode_www_form(value)
  end

  defp decode_pair({key, value}, acc) do
    parts = :binary.split(key, ".", [:global])
    assign_map(parts, value, acc)
  end

  defp assign_map(parts, value, acc) do
    {_, acc} =
      get_and_update_in(acc, Enum.map(parts, &Access.key(&1, %{})), fn
        prev when prev == %{} -> {prev, value}
        prev when is_list(prev) -> {prev, [value | prev]}
        prev -> {prev, [value, prev]}
      end)

    acc
  end
end

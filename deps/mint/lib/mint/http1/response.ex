defmodule Mint.HTTP1.Response do
  @moduledoc false

  alias Mint.Core.Headers

  def decode_status_line(binary) do
    case :erlang.decode_packet(:http_bin, binary, []) do
      {:ok, {:http_response, version, status, reason}, rest} ->
        {:ok, {version, status, reason}, rest}

      {:ok, _other, _rest} ->
        :error

      {:more, _length} ->
        :more

      {:error, _reason} ->
        :error
    end
  end

  def decode_header(binary) do
    case :erlang.decode_packet(:httph_bin, binary, []) do
      {:ok, {:http_header, _unused, name, _reserved, value}, rest} ->
        {:ok, {header_name(name), value}, rest}

      {:ok, :http_eoh, rest} ->
        {:ok, :eof, rest}

      {:ok, _other, _rest} ->
        :error

      {:more, _length} ->
        :more

      {:error, _reason} ->
        :error
    end
  end

  defp header_name(atom) when is_atom(atom), do: atom |> Atom.to_string() |> header_name()
  defp header_name(binary) when is_binary(binary), do: Headers.lower_raw(binary)
end

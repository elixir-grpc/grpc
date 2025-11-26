defmodule Mint.HTTP1.Request do
  @moduledoc false

  import Mint.HTTP1.Parse

  def encode(method, target, headers, body) do
    body = [
      encode_request_line(method, target),
      encode_headers(headers),
      "\r\n",
      encode_body(body)
    ]

    {:ok, body}
  catch
    {:mint, reason} -> {:error, reason}
  end

  defp encode_request_line(method, target) do
    [method, ?\s, target, " HTTP/1.1\r\n"]
  end

  defp encode_headers(headers) do
    Enum.reduce(headers, "", fn {name, value}, acc ->
      validate_header_name!(name)
      validate_header_value!(name, value)
      [acc, name, ": ", value, "\r\n"]
    end)
  end

  defp encode_body(nil), do: ""
  defp encode_body(:stream), do: ""
  defp encode_body(body), do: body

  def encode_chunk(:eof) do
    "0\r\n\r\n"
  end

  def encode_chunk({:eof, trailing_headers}) do
    ["0\r\n", encode_headers(trailing_headers), "\r\n"]
  end

  def encode_chunk(chunk) do
    length = IO.iodata_length(chunk)
    [Integer.to_string(length, 16), "\r\n", chunk, "\r\n"]
  end

  defp validate_header_name!(name) do
    _ =
      for <<char <- name>> do
        unless is_tchar(char) do
          throw({:mint, {:invalid_header_name, name}})
        end
      end

    :ok
  end

  defp validate_header_value!(name, value) do
    _ =
      for <<char <- value>> do
        unless is_vchar(char) or char in ~c"\s\t" do
          throw({:mint, {:invalid_header_value, name, value}})
        end
      end

    :ok
  end
end

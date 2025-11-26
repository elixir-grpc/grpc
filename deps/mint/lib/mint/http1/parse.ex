defmodule Mint.HTTP1.Parse do
  @moduledoc false

  defmacro is_digit(char), do: quote(do: unquote(char) in ?0..?9)
  defmacro is_alpha(char), do: quote(do: unquote(char) in ?a..?z or unquote(char) in ?A..?Z)
  defmacro is_whitespace(char), do: quote(do: unquote(char) in ~c"\s\t")
  defmacro is_comma(char), do: quote(do: unquote(char) == ?,)
  defmacro is_vchar(char), do: quote(do: unquote(char) in 33..126)

  defmacro is_tchar(char) do
    quote do
      is_digit(unquote(char)) or is_alpha(unquote(char)) or unquote(char) in ~c"!#$%&'*+-.^_`|~"
    end
  end

  def ignore_until_crlf(<<>>), do: :more
  def ignore_until_crlf(<<"\r\n", rest::binary>>), do: {:ok, rest}
  def ignore_until_crlf(<<_char, rest::binary>>), do: ignore_until_crlf(rest)

  def content_length_header(string) do
    case Integer.parse(String.trim_trailing(string)) do
      {length, ""} when length >= 0 -> {:ok, length}
      _other -> {:error, {:invalid_content_length_header, string}}
    end
  end

  def connection_header(string) do
    split_into_downcase_tokens(string)
  end

  def transfer_encoding_header(string) do
    split_into_downcase_tokens(string)
  end

  defp split_into_downcase_tokens(string) do
    case token_list_downcase(string) do
      {:ok, []} -> {:error, :empty_token_list}
      {:ok, list} -> {:ok, list}
      :error -> {:error, {:invalid_token_list, string}}
    end
  end

  # Made public for testing.
  def token_list_downcase(string), do: token_list_downcase(string, [])

  defp token_list_downcase(<<>>, acc), do: {:ok, :lists.reverse(acc)}

  # Skip all whitespace and commas.
  defp token_list_downcase(<<char, rest::binary>>, acc)
       when is_whitespace(char) or is_comma(char),
       do: token_list_downcase(rest, acc)

  defp token_list_downcase(rest, acc), do: token_downcase(rest, _token_acc = <<>>, acc)

  defp token_downcase(<<char, rest::binary>>, token_acc, acc) when is_tchar(char),
    do: token_downcase(rest, <<token_acc::binary, downcase_ascii_char(char)>>, acc)

  defp token_downcase(rest, token_acc, acc), do: token_list_sep_downcase(rest, [token_acc | acc])

  defp token_list_sep_downcase(<<>>, acc), do: {:ok, :lists.reverse(acc)}

  defp token_list_sep_downcase(<<char, rest::binary>>, acc) when is_whitespace(char),
    do: token_list_sep_downcase(rest, acc)

  defp token_list_sep_downcase(<<char, rest::binary>>, acc) when is_comma(char),
    do: token_list_downcase(rest, acc)

  defp token_list_sep_downcase(_rest, _acc), do: :error

  defp downcase_ascii_char(char) when char in ?A..?Z, do: char + 32
  defp downcase_ascii_char(char) when char in 0..127, do: char
end

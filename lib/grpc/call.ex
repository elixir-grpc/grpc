defmodule GRPC.Call do
  def unary(channel, path, message, opts) do
    headers = compose_headers(channel, path, opts)
    {:ok, body} = GRPC.Message.delimited(message, opts)
    :h2_client.sync_request(channel.pid, headers, body)
  end

  defp compose_headers(channel, path, opts) do
    [
      {":method", "POST"},
      {":scheme", channel.scheme},
      {":path", path},
      {":authority", channel.host},
      {"content-type", "application/grpc"},
      {"user-agent", GRPC.version},
      {"te", "trailers"}
    ]
    |> append_encoding(Keyword.get(opts, :send_encoding))
    |> append_timeout(Keyword.get(opts, :timeout))
    |> append_custom_metadata(Keyword.get(opts, :metadata))
    # TODO: Authorization
  end

  defp append_encoding(headers, send_encoding) when is_binary(send_encoding) do
    headers ++ [{"grpc-encoding", send_encoding}]
  end
  defp append_encoding(headers, _), do: headers

  defp append_timeout(headers, timeout) when is_integer(timeout) and timeout > 0 do
    headers ++ [{"grpc-timeout", encode_timeout(timeout)}]
  end
  defp append_timeout(headers, _), do: headers

  defp append_custom_metadata(headers, metadata) when is_map(metadata) and map_size(metadata) > 0 do
    new_headers = Enum.filter_map(metadata, fn({k, _v})-> !is_reserved_header(to_string(k)) end,
                                            fn({k, v})-> {String.downcase(to_string(k)), to_string(v)} end)
    headers ++ new_headers
  end
  defp append_custom_metadata(headers, _), do: headers

  defp encode_timeout(_timeout) do
    # TODO
  end

  defp is_reserved_header(":" <> _), do: true
  defp is_reserved_header("grpc-" <> _), do: true
  defp is_reserved_header("content-type"), do: true
  defp is_reserved_header("te"), do: true
  defp is_reserved_header(_), do: false
end

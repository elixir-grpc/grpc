defmodule GRPC.Transport.HTTP2 do
  @moduledoc """
  A module providing functions for sending HTTP/2 requests.
  """

  alias GRPC.Transport.Utils
  alias GRPC.Status

  def server_headers() do
    %{":status" => 200, "content-type" => "application/grpc+proto"}
  end

  @spec server_trailers(String.t(), GRPC.Status) :: [{String.t(), String.t()}]
  def server_trailers(status \\ Status.ok(), message \\ "") do
    %{
      "grpc-status" => Integer.to_string(status),
      "grpc-message" => message
    }
  end

  @spec client_headers(GRPC.Client.Stream.t(), keyword) :: [{String.t(), String.t()}]
  def client_headers(%{channel: channel, path: path} = s, opts \\ []) do
    [
      {":method", "POST"},
      {":scheme", channel.scheme},
      {":path", path},
      {":authority", channel.host},
    ] ++ client_headers_without_reserved(s, opts)
  end

  @spec client_headers(GRPC.Client.Stream.t(), keyword) :: [{String.t(), String.t()}]
  def client_headers_without_reserved(_, opts \\ []) do
    [
      {"content-type", opts[:content_type] || "application/grpc+proto"},
      {"user-agent", "grpc-elixir/#{opts[:grpc_version] || GRPC.version()}"},
      {"te", "trailers"}
    ]
    |> append_encoding(Keyword.get(opts, :send_encoding))
    |> append_timeout(Keyword.get(opts, :deadline) || Keyword.get(opts, :timeout))
    |> append_custom_metadata(Keyword.get(opts, :metadata))

    # TODO: grpc-accept-encoding, grpc-message-type
    # TODO: Authorization
  end

  defp append_encoding(headers, send_encoding) when is_binary(send_encoding) do
    headers ++ [{"grpc-encoding", send_encoding}]
  end

  defp append_encoding(headers, _), do: headers

  defp append_timeout(headers, timeout) when is_integer(timeout) do
    headers ++ [{"grpc-timeout", Utils.encode_timeout(timeout)}]
  end

  defp append_timeout(headers, deadline) when deadline != nil do
    headers ++ [{"grpc-timeout", Utils.encode_timeout(deadline)}]
  end

  defp append_timeout(headers, _), do: headers

  defp append_custom_metadata(headers, metadata)
       when is_map(metadata) and map_size(metadata) > 0 do
    new_headers =
      metadata
      |> Enum.filter(fn {k, _v} -> !is_reserved_header(to_string(k)) end)
      |> Enum.map(fn {k, v} -> normalize_custom_metadata({k, v}) end)

    headers ++ new_headers
  end

  defp append_custom_metadata(headers, _), do: headers

  defp normalize_custom_metadata({key, val}) when not is_binary(key) do
    normalize_custom_metadata({to_string(key), val})
  end

  defp normalize_custom_metadata({key, val}) when not is_binary(val) do
    normalize_custom_metadata({key, to_string(val)})
  end

  defp normalize_custom_metadata({key, val}) do
    val = if String.ends_with?(key, "-bin"), do: Base.encode64(val), else: val
    {String.downcase(to_string(key)), val}
  end

  defp is_reserved_header(":" <> _), do: true
  defp is_reserved_header("grpc-" <> _), do: true
  defp is_reserved_header("content-type"), do: true
  defp is_reserved_header("te"), do: true
  defp is_reserved_header(_), do: false
end

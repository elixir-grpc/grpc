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

  @spec client_headers(GRPC.Client.Stream.t(), map) :: [{String.t(), String.t()}]
  def client_headers(%{channel: channel, path: path} = s, opts \\ %{}) do
    [
      {":method", "POST"},
      {":scheme", channel.scheme},
      {":path", path},
      {":authority", channel.host},
    ] ++ client_headers_without_reserved(s, opts)
  end

  @spec client_headers(GRPC.Client.Stream.t(), map) :: [{String.t(), String.t()}]
  def client_headers_without_reserved(_, opts \\ %{}) do
    [
      {"content-type", opts[:content_type] || "application/grpc+proto"},
      {"user-agent", "grpc-elixir/#{opts[:grpc_version] || GRPC.version()}"},
      {"te", "trailers"}
    ]
    |> append_encoding(opts[:grpc_encoding])
    |> append_timeout(opts[:timeout])
    |> append_custom_metadata(opts[:metadata])

    # TODO: grpc-accept-encoding, grpc-message-type
    # TODO: Authorization
  end

  def extract_metadata(headers) do
    headers
    |> Enum.filter(fn({k, _}) -> is_metadata(k) end)
    |> Enum.into(%{})
  end

  defp append_encoding(headers, grpc_encoding) when is_binary(grpc_encoding) do
    headers ++ [{"grpc-encoding", grpc_encoding}]
  end

  defp append_encoding(headers, _), do: headers

  defp append_timeout(headers, timeout) when is_integer(timeout) do
    headers ++ [{"grpc-timeout", Utils.encode_timeout(timeout)}]
  end
  defp append_timeout(headers, _), do: headers

  defp append_custom_metadata(headers, metadata) when is_map(metadata) or is_list(metadata) do
    new_headers =
      metadata
      |> Enum.filter(fn {k, _v} -> !is_reserved_header(to_string(k)) end)
      |> Enum.reduce(%{}, fn({k, v}, acc) ->
        {new_k, new_v} = normalize_custom_metadata({k, v})
        Map.update(acc, new_k, new_v, fn old_v -> Enum.join([old_v, new_v], ",") end)
      end)
      |> Map.to_list

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

  defp is_metadata("content-length"), do: false
  defp is_metadata("user-agent"), do: false
  defp is_metadata(key) do
    !is_reserved_header(key)
  end
end

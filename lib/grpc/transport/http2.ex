defmodule GRPC.Transport.HTTP2 do
  @moduledoc false

  # A module providing functions for sending HTTP/2 requests.

  alias GRPC.Transport.Utils
  alias GRPC.Status

  require Logger

  def server_headers(%{codec: GRPC.Codec.WebText = codec}) do
    %{"content-type" => "application/grpc-web-#{codec_name(codec)}"}
  end

  # TO-DO: refactor when we add a GRPC.Codec.content_type callback
  def server_headers(%{codec: GRPC.Codec.JSON}) do
    %{"content-type" => "application/json"}
  end

  def server_headers(%{codec: codec}) do
    %{"content-type" => "application/grpc+#{codec_name(codec)}"}
  end

  @spec server_trailers(integer, String.t(), [Google.Protobuf.Any.t()] | nil) :: map
  def server_trailers(status \\ Status.ok(), message \\ "", details \\ nil) do
    %{
      "grpc-status" => Integer.to_string(status),
      "grpc-message" => URI.encode(message)
    }
    |> put_details_bin_grpc_status(status, message, details)
  end

  defp put_details_bin_grpc_status(trailers, _status, _message, nil), do: trailers
  defp put_details_bin_grpc_status(trailers, _status, _message, []), do: trailers

  defp put_details_bin_grpc_status(trailers, status, message, details) when is_list(details) do
    encoded_details =
      GRPC.Google.RPC.encode_status(%Google.Rpc.Status{
        code: status,
        message: message,
        details: details
      })

    Map.put(trailers, "grpc-status-details-bin", encoded_details)
  end

  @doc """
  Now we may not need this because gun already handles the pseudo headers.
  """
  @spec client_headers(GRPC.Client.Stream.t(), keyword()) :: [{String.t(), String.t()}]
  def client_headers(%{channel: channel, path: path} = s, opts \\ []) do
    [
      {":method", "POST"},
      {":scheme", channel.scheme},
      {":path", path},
      {":authority", channel.host}
    ] ++ client_headers_without_reserved(s, opts)
  end

  @spec client_headers_without_reserved(GRPC.Client.Stream.t(), keyword()) :: [
          {String.t(), String.t()}
        ]
  def client_headers_without_reserved(%{codec: codec} = stream, opts \\ []) do
    [
      # It seems only gRPC implemenations only support "application/grpc", so we support :content_type now.
      {"content-type", content_type(opts[:content_type], codec)},
      {"user-agent", "grpc-elixir/#{opts[:grpc_version] || GRPC.version()}"},
      {"te", "trailers"}
    ]
    |> append_compressor(stream.compressor)
    |> append_accepted_compressors(stream.accepted_compressors)
    |> append_custom_metadata(stream.channel.headers)
    |> append_encoding(opts[:grpc_encoding])
    |> append_timeout(opts[:timeout])
    |> append_custom_metadata(stream.headers)
    |> append_custom_metadata(opts[:metadata])

    # TODO: grpc-accept-encoding, grpc-message-type
    # TODO: Authorization
  end

  defp content_type(custom, _codec) when is_binary(custom), do: custom

  # Some gRPC implementations don't support application/grpc+xyz,
  # to avoid this kind of trouble, use application/grpc by default
  defp content_type(_, GRPC.Codec.Proto), do: "application/grpc"

  defp content_type(_, codec = GRPC.Codec.WebText),
    do: "application/grpc-web-#{codec_name(codec)}"

  defp content_type(_, codec), do: "application/grpc+#{codec_name(codec)}"

  def extract_metadata(headers) do
    headers
    |> Enum.filter(fn {k, _} -> is_metadata(k) end)
    |> Enum.into(%{}, &decode_metadata/1)
  end

  def decode_headers(headers) do
    Enum.into(headers, %{}, fn {k, v} ->
      if is_metadata(k) do
        decode_metadata({k, v})
      else
        decode_reserved({k, v})
      end
    end)
  end

  def encode_metadata(metadata) do
    metadata
    |> Enum.filter(fn {k, _v} -> !is_reserved_header(to_string(k)) end)
    |> Enum.reduce(%{}, fn {k, v}, acc ->
      {new_k, new_v} = encode_metadata_pair({k, v})
      Map.update(acc, new_k, new_v, fn old_v -> Enum.join([old_v, new_v], ",") end)
    end)
  end

  defp append_encoding(headers, grpc_encoding) when is_binary(grpc_encoding) do
    Logger.warning("grpc_encoding option is deprecated, please use compressor.")
    [{"grpc-encoding", grpc_encoding} | headers]
  end

  defp append_encoding(headers, _), do: headers

  defp append_compressor(headers, compressor) when not is_nil(compressor) do
    [{"grpc-encoding", compressor_name(compressor)} | headers]
  end

  defp append_compressor(headers, _), do: headers

  defp append_accepted_compressors(headers, [_] = compressors) do
    encoding = Enum.map_join(compressors, ",", &compressor_name/1)
    [{"grpc-accept-encoding", encoding} | headers]
  end

  defp append_accepted_compressors(headers, _), do: headers

  defp append_timeout(headers, timeout) when is_integer(timeout) do
    [{"grpc-timeout", Utils.encode_timeout(timeout)} | headers]
  end

  defp append_timeout(headers, _), do: headers

  defp append_custom_metadata(headers, metadata) when is_map(metadata) or is_list(metadata) do
    Enum.to_list(encode_metadata(metadata)) ++ headers
  end

  defp append_custom_metadata(headers, _), do: headers

  defp encode_metadata_pair({key, val}) when not is_binary(key) do
    encode_metadata_pair({to_string(key), val})
  end

  defp encode_metadata_pair({key, val}) when not is_binary(val) do
    encode_metadata_pair({key, to_string(val)})
  end

  defp encode_metadata_pair({key, val}) do
    val = if String.ends_with?(key, "-bin"), do: Base.encode64(val, padding: true), else: val
    {String.downcase(to_string(key)), val}
  end

  defp decode_metadata({key, val}) do
    val = if String.ends_with?(key, "-bin"), do: Base.decode64!(val, padding: false), else: val
    {key, val}
  end

  defp decode_reserved({"grpc-message" = key, val}) do
    {key, URI.decode(val)}
  end

  defp decode_reserved(kv), do: kv

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

  defp codec_name(codec) when is_atom(codec), do: apply(codec, :name, [])
  defp codec_name(%{name: name}), do: name

  defp compressor_name(compressor) when is_atom(compressor), do: apply(compressor, :name, [])
  defp compressor_name(%{name: name}), do: name
end

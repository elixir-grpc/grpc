defmodule GRPC.Client.Connection.EndpointResolver do
  @moduledoc false

  # Parses and normalises a raw target string into the canonical
  # `{norm_target, scheme, cred}` triple consumed by `GRPC.Client.Connection`.
  #
  # Supported input formats:
  #
  #   * `"https://host:port"` – TLS, implicit :cred injection
  #   * `"http://host:port"`  – plain-text, rejects :cred
  #   * `"host:port"`         – compatibility shorthand → ipv4/ipv6
  #   * `"path"`              – bare path → unix socket
  #   * `"dns://…"`, `"ipv4:…"`, `"ipv6:…"`, `"unix:…"`, etc. – passed through
  #   * `"[::1]:port"`        – bracketed IPv6 with port → ipv6 normalised
  #   * `"::1:port"`          – bare IPv6 with port → ipv6 normalised

  @insecure_scheme "http"
  @secure_scheme "https"
  @default_port 50051

  @doc """
  Normalises `target` and `cred`, returning `{norm_target, scheme, cred}`.

  - `norm_target` – canonical target string for the resolver (e.g. `"ipv4:1.2.3.4:50051"`, `"ipv6:::1:50051"`)
  - `scheme`      – `"http"`, `"https"`, or `"unix"`
  - `cred`        – resolved `%GRPC.Credential{}`, or `nil` for plain-text targets

  ## Examples

      iex> GRPC.Client.Connection.EndpointResolver.normalize("http://example.com:50051", nil)
      {"ipv4:example.com:50051", "http", nil}

      iex> cred = %GRPC.Credential{ssl: [verify: :verify_none]}
      iex> GRPC.Client.Connection.EndpointResolver.normalize("https://example.com:50051", cred)
      {"ipv4:example.com:50051", "https", %GRPC.Credential{ssl: [verify: :verify_none]}}

      iex> GRPC.Client.Connection.EndpointResolver.normalize("localhost:50051", nil)
      {"ipv4:localhost:50051", "http", nil}

      iex> GRPC.Client.Connection.EndpointResolver.normalize("[::1]:50051", nil)
      {"ipv6:::1:50051", "http", nil}

  """
  @spec normalize(String.t(), GRPC.Credential.t() | nil) ::
          {String.t(), String.t(), GRPC.Credential.t() | nil}
  def normalize(target, cred)
      when is_binary(target) and (is_nil(cred) or is_struct(cred, GRPC.Credential)) do
    uri = URI.parse(target)

    cond do
      uri.scheme == @secure_scheme and uri.host ->
        resolved_cred = cred || default_ssl_option()
        prefix = resolver_prefix(uri.host)
        {"#{prefix}:#{uri.host}:#{uri.port}", @secure_scheme, resolved_cred}

      uri.scheme == @insecure_scheme and uri.host ->
        if cred,
          do: raise(ArgumentError, "invalid option for insecure (http) address: :cred")

        prefix = resolver_prefix(uri.host)
        {"#{prefix}:#{uri.host}:#{uri.port}", @insecure_scheme, nil}

      # Compatibility mode: "host:port", bare path, or raw IPv6
      uri.scheme in [nil, ""] ->
        scheme = if cred, do: @secure_scheme, else: @insecure_scheme
        normalize_schemeless(target, scheme, cred)

      # URI.parse misreads "hostname:port" as scheme="hostname", host=nil.
      # Detect this: a real resolver scheme always has a host OR uses "://"
      # notation. If host is nil and the scheme is not a known gRPC resolver
      # prefix, treat it as a schemeless host:port shorthand.
      is_nil(uri.host) and uri.scheme not in ["ipv4", "ipv6", "dns", "unix", "xds"] ->
        scheme = if cred, do: @secure_scheme, else: @insecure_scheme
        normalize_schemeless(target, scheme, cred)

      true ->
        scheme = if cred, do: @secure_scheme, else: @insecure_scheme
        {target, scheme, cred}
    end
  end

  @doc """
  Splits a resolved target string (e.g. `"ipv4:1.2.3.4:50051"`) into
  `{host, port}`.

  Handles:
  - `"host:port"` → `{"host", port}`
  - `"scheme:host:port"` → `{"host", port}`
  - `"host"` → `{"host", #{@default_port}}`
  - `"[::1]:port"` → `{"::1", port}` (bracketed IPv6)
  - `"::1:port"` → `{"::1", port}` (bare IPv6, port is the last segment)

  ## Examples

      iex> GRPC.Client.Connection.EndpointResolver.split_host_port("ipv4:127.0.0.1:50051")
      {"127.0.0.1", 50051}

      iex> GRPC.Client.Connection.EndpointResolver.split_host_port("localhost:8080")
      {"localhost", 8080}

      iex> GRPC.Client.Connection.EndpointResolver.split_host_port("[::1]:50051")
      {"::1", 50051}

      iex> GRPC.Client.Connection.EndpointResolver.split_host_port("myhost")
      {"myhost", 50051}

  """
  @spec split_host_port(String.t()) :: {String.t(), pos_integer()}
  def split_host_port(target) when is_binary(target) do
    cond do
      String.contains?(target, "[") ->
        case Regex.run(~r/\[([^\]]+)\]:(\d+)$/, target) do
          [_, addr, port] ->
            {addr, String.to_integer(port)}

          _ ->
            case Regex.run(~r/\[([^\]]+)\]/, target) do
              [_, addr] -> {addr, @default_port}
              _ -> {strip_scheme(target), @default_port}
            end
        end

      target |> String.split(":") |> length() > 2 ->
        parts = String.split(target, ":")

        case {parts, Integer.parse(List.last(parts))} do
          {[_scheme, host, port_str], {_port, ""}} ->
            {host, String.to_integer(port_str)}

          {_, {_port, ""}} ->
            port_str = List.last(parts)
            addr = parts |> Enum.drop(-1) |> Enum.join(":")
            {addr, String.to_integer(port_str)}

          {[_scheme, host], _} ->
            {host, @default_port}

          _ ->
            {strip_scheme(target), @default_port}
        end

      String.contains?(target, ":") ->
        [h, p] = String.split(target, ":", parts: 2)

        case Integer.parse(p) do
          {port, ""} -> {h, port}
          _ -> {p, @default_port}
        end

      true ->
        {target, @default_port}
    end
  end

  defp normalize_schemeless(target, scheme, cred) do
    cond do
      String.starts_with?(target, "[") ->
        case Regex.run(~r/^\[([^\]]+)\]:(\d+)$/, target) do
          [_, addr, port] ->
            {"ipv6:#{addr}:#{port}", scheme, cred}

          _ ->
            addr = target |> String.trim_leading("[") |> String.replace("]", "")
            {"ipv6:#{addr}", scheme, cred}
        end

      String.contains?(target, ":") ->
        parts = String.split(target, ":")

        case List.last(parts) do
          port_str when byte_size(port_str) > 0 ->
            case Integer.parse(port_str) do
              {_port, ""} ->
                addr = parts |> Enum.drop(-1) |> Enum.join(":")
                prefix = resolver_prefix(addr)
                {"#{prefix}:#{addr}:#{port_str}", scheme, cred}

              _ ->
                prefix = resolver_prefix(target)
                {"#{prefix}:#{target}", scheme, cred}
            end

          _ ->
            prefix = resolver_prefix(target)
            {"#{prefix}:#{target}", scheme, cred}
        end

      true ->
        {"unix://#{target}", "unix", nil}
    end
  end

  defp resolver_prefix(host) when is_binary(host) do
    if String.contains?(host, ":"), do: "ipv6", else: "ipv4"
  end

  defp strip_scheme(target) do
    case String.split(target, ":", parts: 2) do
      [_scheme, rest] -> rest
      [bare] -> bare
    end
  end

  if {:module, CAStore} == Code.ensure_loaded(CAStore) do
    defp default_ssl_option do
      %GRPC.Credential{
        ssl: [
          verify: :verify_peer,
          depth: 99,
          cacertfile: CAStore.file_path()
        ]
      }
    end
  else
    defp default_ssl_option do
      raise """
      no GRPC credentials provided. Please either:

      - Pass the `:cred` option to `GRPC.Stub.connect/2,3`
      - Add `:castore` to your list of dependencies in `mix.exs`
      """
    end
  end
end

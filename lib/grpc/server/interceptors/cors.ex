defmodule GRPC.Server.Interceptors.CORS do
  @moduledoc """
  Sends CORS headers when the client is using RPC via Web transcoding or gRPC-web.

  ## Options

    * `:allow_origin` - Required. A string containing the allowed origin, or a function capture (e.g. `&MyApp.MyModule.function/2)`) which takes a `req` and a `stream` and returns a string.
    * `:allow_headers` - A string containing the allowed headers, or a function capture
      (e.g. `&MyApp.MyModule.function/2)`) which takes a `req` and a `stream` and returns a string. Defaults to `nil`.
      If defined as `nil`, the value of the `"access-control-request-headers"` request header from the client will be used in the response.

  ## Usage

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Server.Interceptors.CORS
      end

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Server.Interceptors.CORS, allow_origin: "some.origin"
      end


      defmodule Your.Endpoint do
        use GRPC.Endpoint

        def allow_origin(req, stream), do: "calculated.origin"
        intercept GRPC.Server.Interceptors.CORS, allow: &Your.Endpoint.allow_origin/2
      end
  """

  @behaviour GRPC.Server.Interceptor
  @impl true
  def init(opts \\ []) do
    # Function captures are represented as their AST in this step
    # because of a Macro.escape call in the __before_compile__ step
    # in endpoint.ex.
    # This is not a full-on Macro context, so binary concatenations and
    # variables are handled before this step.

    opts = Keyword.validate!(opts, [:allow_origin, allow_headers: nil])

    allow_origin =
      case opts[:allow_origin] do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun ->
          fun

        binary when is_binary(binary) ->
          binary

        other ->
          raise ArgumentError,
                "allow_origin must be a string or a 2-arity remote function, got: #{inspect(other)}"
      end

    allow_headers =
      case opts[:allow_headers] do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun ->
          fun

        binary when is_binary(binary) ->
          binary

        nil ->
          nil

        other ->
          raise ArgumentError,
                ":allow_headers must be a string, a 2-arity remote function, or nil, got: #{inspect(other)}"
      end

    {allow_origin, allow_headers}
  end

  @impl true
  def call(req, stream, next, {allow_origin, allow_headers}) do
    if stream.access_mode != :grpc do
      headers =
        %{}
        |> add_allowed_origins(req, stream, allow_origin)
        |> add_allowed_headers(req, stream, allow_headers)

      stream.adapter.set_headers(stream.payload, headers)
    end

    next.(req, stream)
  end

  defp add_allowed_origins(headers, req, stream, allow) do
    value =
      case allow do
        allow when is_function(allow, 2) -> allow.(req, stream)
        allow -> allow
      end

    Map.put(headers, "access-control-allow-origin", value)
  end

  defp add_allowed_headers(
         headers,
         req,
         %{http_request_headers: %{"access-control-request-headers" => requested}} = stream,
         allow
       ) do
    # include an access-control-allow-headers header only when a request headers is sent
    value =
      case allow do
        nil -> requested
        allow when is_function(allow, 2) -> allow.(req, stream)
        allow -> allow
      end

    Map.put(headers, "access-control-allow-headers", value)
  end

  defp add_allowed_headers(headers, _req, _stream, _allowed), do: headers
end

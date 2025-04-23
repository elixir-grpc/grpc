defmodule GRPC.Server.Interceptors.CORS do
  @moduledoc """
  Sends CORS headers when the client is calling the rpc via web transcoding or grpcweb.

  ## Options

    * `allow_origin` - A string containing the allowed origin(s), or a remote function (e.g. `&MyApp.MyModule.function/2)`) which takes a `req` and a `stream` and returns a string. Defaults to `"*"`, which will allow all origins to access this endpoint.
    * `allow_headers` - A string containing the allowed headers, or a remote function (e.g. `&MyApp.MyModule.function/2)`) which takes a `req` and a `stream` and returns a string. Defaults to the value of the `"access-control-request-headers"` request header from the client.

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
    # the funky first clause matches a 2-arity remote is_function
    # note that this function is run in the context of a macro, which brings some limitations with it
    allow_origin =
      case Keyword.get(opts, :allow_origin) do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun -> fun
        static when is_binary(static) -> static
        _ -> "*"
      end

    allow_headers =
      case Keyword.get(opts, :allow_headers) do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun -> fun
        static when is_binary(static) -> static
        _ -> nil
      end

    {allow_origin, allow_headers}
  end

  @impl true
  def call(req, stream, next, {allow_origin, allow_headers}) do
    if stream.access_mode != :grpc and
         Map.get(stream.http_request_headers, "sec-fetch-mode") == "cors" do
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

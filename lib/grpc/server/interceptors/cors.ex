defmodule GRPC.Server.Interceptors.CORS do
  @moduledoc """
  Sends CORS headers when the client is calling the rpc via web transcoding or grpcweb.

  ## Options

    * `allowed` - A string contianing the allowed origin(s), or a remote function (e.g. `&MyApp.MyModule.function/2)`) which takes a `req` and a `stream` and returns a string. Defaults to `"*"`, which will allow all origins to access this endpoint.

  ## Usage

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Server.Interceptors.CORS
      end

      defmodule Your.Endpoint do
        use GRPC.Endpoint

        intercept GRPC.Server.Interceptors.CORS, allowed: "some.origin"
      end


      defmodule Your.Endpoint do
        use GRPC.Endpoint

        def allowed_origin(req, stream), do: "calculated.origin"
        intercept GRPC.Server.Interceptors.CORS, allowed: &Your.Endpoint.allowed_origin/2
      end
  """

  @behaviour GRPC.Server.Interceptor
  @impl true
  def init(opts \\ []) do
    # the funky first clause matches a 2-arity remote is_function
    # note that this function is run in the context of a macro, which brings some limitations with it
    allowed_origin =
      case Keyword.get(opts, :allow_origin) do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun -> fun
        static when is_binary(static) -> static
        _ -> "*"
      end

    allowed_headers =
      case Keyword.get(opts, :allow_headers) do
        {:&, [], [{:/, [], [_signature, 2]}]} = fun -> fun
        static when is_binary(static) -> static
        _ -> nil
      end

    {allowed_origin, allowed_headers}
  end

  @impl true
  def call(req, stream, next, {allowed_origin, allowed_headers}) do
    if stream.access_mode != :grpc and
         Map.get(stream.http_request_headers, "sec-fetch-mode") == "cors" do
      headers =
        %{}
        |> add_allowed_origins(req, stream, allowed_origin)
        |> add_allowed_headers(req, stream, allowed_headers)

      stream.adapter.set_headers(stream.payload, headers)
    end

    next.(req, stream)
  end

  defp add_allowed_origins(headers, req, stream, allowed) do
    value =
      case allowed do
        allowed when is_function(allowed, 2) -> allowed.(req, stream)
        allowed -> allowed
      end

    Map.put(headers, "access-control-allow-origin", value)
  end

  defp add_allowed_headers(
         headers,
         req,
         %{http_request_headers: %{"access-control-request-headers" => requested}} = stream,
         allowed
       ) do
    # include an access-control-allow-headers header only when a request headers is sent
    value =
      case allowed do
        nil -> requested
        allowed when is_function(allowed, 2) -> allowed.(req, stream)
        allowed -> allowed
      end

    Map.put(headers, "access-control-allow-headers", value)
  end

  defp add_allowed_headers(headers, _req, _stream, _allowed), do: headers
end

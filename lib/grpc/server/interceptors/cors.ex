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
  def init(opts) do
    # the funky first clause matches a 2-arity remote is_function
    # note that this function is run in the context of a macro, which brings some limitations with it
    case Keyword.get(opts, :allow) do
      {:&, [], [{:/, [], [_signature, 2]}]} = fun -> fun
      static when is_binary(static) -> static
      _ ->"*"
    end
  end

  @impl true
  def call(req, stream, next, allowed) do
    if stream.access_mode != :grpc do
      stream.adapter.set_headers(stream.payload, %{
        "access-control-allow-origin" => resolve_allowed(req, stream, allowed),
        "access-control-allow-headers" => "content-type, x-grpc-web, x-user-agent, x-api-key"
      })
    end

    next.(req, stream)
  end

  defp resolve_allowed(req, stream, allowed_fn) when is_function(allowed_fn, 2) do
    allowed_fn.(req, stream)
  end

  defp resolve_allowed(_req, _stream, allowed), do: allowed
end

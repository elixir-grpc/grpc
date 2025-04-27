defmodule GRPC.Server.Interceptors.CORSTest.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Server.Interceptors.CORS,
    allow_origin: &GRPC.Server.Interceptors.CORSTest.allow_origin/2,
    allow_headers: &GRPC.Server.Interceptors.CORSTest.allow_headers/2
  )
end

defmodule GRPC.Server.Interceptors.CORSTest do
  use ExUnit.Case, async: false

  alias GRPC.Server.Interceptors.CORS, as: CORSInterceptor
  alias GRPC.Server.Stream

  defmodule FakeRequest do
    defstruct []
  end

  @server_name :server
  @rpc {1, 2, 3}
  @adaptor GRPC.Test.ServerAdapter
  @function_header_value "from-function"
  @default_http_headers %{
    "accept" => "application/grpc-web-text",
    "accept-encoding" => "gzip, deflate, br, zstd",
    "accept-language" => "en-US,en;q=0.5",
    "connection" => "keep-alive",
    "content-length" => "20",
    "content-type" => "application/grpc-web-text",
    "dnt" => "1",
    "host" => "http://myhost:4100",
    "priority" => "u=0",
    "referer" => "http://localhost:3000/",
    "sec-fetch-dest" => "empty",
    "sec-fetch-mode" => "cors",
    "sec-fetch-site" => "same-site",
    "user-agent" => "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0",
    "x-grpc-web" => "1",
    "x-user-agent" => "grpc-web-javascript/0.1"
  }
  @requested_allowed_headers "Authorized"
  @custom_allowed_headers "MySpecialHeader,AndAnother"

  def allow_origin(_req, _stream), do: @function_header_value
  def allow_headers(_req, _stream), do: @custom_allowed_headers

  def create_stream() do
    %Stream{
      adapter: @adaptor,
      server: @server_name,
      rpc: @rpc,
      http_request_headers: @default_http_headers
    }
  end

  test "Sends headers CORS for for http transcoding and grpcweb requests" do
    request = %FakeRequest{}
    stream = create_stream()

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :http_transcode},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    assert_received({:setting_headers, _headers}, "Failed to set CORS headers during grpcweb")

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    assert_received({:setting_headers, _headers}, "Failed to set CORS headers during grpcweb")
  end

  test "Does not send CORS headers for normal grpc requests" do
    request = %FakeRequest{}
    stream = create_stream()

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpc},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    refute_received({:setting_headers, _headers}, "Set CORS headers during grpc")
  end

  test "CORS allow origin header value is configuraable with a static string" do
    request = %FakeRequest{}
    stream = Map.put(create_stream(), :access_mode, :grpcweb)
    domain = "https://mydomain.io"

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: domain)
      )

    assert_received(
      {:setting_headers, %{"access-control-allow-origin" => ^domain}},
      "Incorrect static header"
    )
  end

  test "CORS allow origin init does not accept non-string arguments" do
    assert_raise(ArgumentError, fn -> CORSInterceptor.init(allow_origin: :atom) end)
    assert_raise(ArgumentError, fn -> CORSInterceptor.init(allow_origin: 1) end)
    assert_raise(ArgumentError, fn -> CORSInterceptor.init(allow_origin: 1.0) end)
    assert_raise(ArgumentError, fn -> CORSInterceptor.init(allow_origin: []) end)
    assert_raise(ArgumentError, fn -> CORSInterceptor.init(allow_origin: %{}) end)
  end

  test "CORS allow origin header value is configuraable with a two-arity function" do
    request = %FakeRequest{}
    stream = Map.put(create_stream(), :access_mode, :grpcweb)

    # fetch the interceptor state from the fake endpoint
    [{_interceptor, interceptor_state}] =
      GRPC.Server.Interceptors.CORSTest.Endpoint.__meta__(:interceptors)

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        interceptor_state
      )

    assert_received(
      {:setting_headers, %{"access-control-allow-origin" => @function_header_value}},
      "Incorrect header when using function"
    )
  end

  test "CORS Access-Control-Allowed-Headers is included in response when clients request it" do
    request = %FakeRequest{}

    stream = %{
      create_stream()
      | access_mode: :grpcweb,
        http_request_headers:
          Map.put(
            @default_http_headers,
            "access-control-request-headers",
            @requested_allowed_headers
          )
    }

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    assert_received(
      {:setting_headers, %{"access-control-allow-headers" => @requested_allowed_headers}},
      "Incorrect header when using function"
    )
  end

  test "CORS Access-Control-Allowed-Headers is configurable with a static string" do
    request = %FakeRequest{}

    stream = %{
      create_stream()
      | access_mode: :grpcweb,
        http_request_headers:
          Map.put(
            @default_http_headers,
            "access-control-request-headers",
            @requested_allowed_headers
          )
    }

    allowed_headers = "Test"

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*", allow_headers: allowed_headers)
      )

    assert_received(
      {:setting_headers, %{"access-control-allow-headers" => ^allowed_headers}},
      "Incorrect header when using function"
    )
  end

  test "CORS Access-Control-Allowed-Headers is configurable with a two-arity function" do
    request = %FakeRequest{}

    stream = %{
      create_stream()
      | access_mode: :grpcweb,
        http_request_headers:
          Map.put(
            @default_http_headers,
            "access-control-request-headers",
            @requested_allowed_headers
          )
    }

    # fetch the interceptor state from the fake endpoint
    [{_interceptor, interceptor_state}] =
      GRPC.Server.Interceptors.CORSTest.Endpoint.__meta__(:interceptors)

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        interceptor_state
      )

    assert_received(
      {:setting_headers, %{"access-control-allow-headers" => @custom_allowed_headers}},
      "Incorrect header when using function"
    )
  end

  test "CORS only on cors sec-fetch-mode" do
    request = %FakeRequest{}

    stream = %{
      create_stream()
      | access_mode: :grpcweb,
        http_request_headers: Map.put(@default_http_headers, "sec-fetch-mode", "same-origin")
    }

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    refute_received({:setting_headers, _}, "Set CORS header")
  end

  test "No CORS if missing sec-fetch-mode header" do
    request = %FakeRequest{}

    stream = %{
      create_stream()
      | access_mode: :grpcweb,
        http_request_headers: Map.delete(@default_http_headers, "sec-fetch-mode")
    }

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow_origin: "*")
      )

    refute_received({:setting_headers, _}, "Set CORS header")
  end
end

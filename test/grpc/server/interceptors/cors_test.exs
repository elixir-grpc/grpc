defmodule GRPC.Server.Interceptors.CORSTest.Endpoint do
  use GRPC.Endpoint
  intercept(GRPC.Server.Interceptors.CORS, allow: &GRPC.Server.Interceptors.CORSTest.allow_origin/2)
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

  def allow_origin(_req, _stream), do: @function_header_value

  test "Sends headers CORS for for http transcoding and grpcweb requests" do
    request = %FakeRequest{}
    stream = %Stream{adapter: @adaptor, server: @server_name, rpc: @rpc}

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :http_transcode},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init()
      )

    assert_received({:setting_headers, _headers}, "Failed to set CORS headers during grpcweb")

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init()
      )

    assert_received({:setting_headers, _headers}, "Failed to set CORS headers during grpcweb")
  end

  test "Does not send CORS headers for normal grpc requests" do
    request = %FakeRequest{}
    stream = %Stream{adapter: @adaptor, server: @server_name}

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpc},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init()
      )

    refute_received({:setting_headers, _headers}, "Set CORS headers during grpc")
  end

  test "Default CORS allow origin header allows all" do
    request = %FakeRequest{}
    stream = %Stream{adapter: @adaptor, server: @server_name, access_mode: :grpcweb}

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        stream,
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init()
      )
    assert_received({:setting_headers, %{"access-control-allow-origin" => "*"}}, "Incorrect default header")
  end

  test "CORS allow origin header value is configuraable with a static string" do
    request = %FakeRequest{}
    stream = %Stream{adapter: @adaptor, server: @server_name, access_mode: :grpcweb}
    domain = "https://mydomain.io"

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        CORSInterceptor.init(allow: domain)
      )

    assert_received({:setting_headers, %{"access-control-allow-origin" => ^domain}}, "Incorrect static header")
  end

  test "CORS allow origin header value is configuraable with a two-arity function" do
    request = %FakeRequest{}
    stream = %Stream{adapter: @adaptor, server: @server_name, access_mode: :grpcweb}

    # fetch the interceptor state from the fake endpoint
    [{_interceptor, interceptor_state}] = GRPC.Server.Interceptors.CORSTest.Endpoint.__meta__(:interceptors)

    {:ok, :ok} =
      CORSInterceptor.call(
        request,
        %{stream | access_mode: :grpcweb},
        fn _request, _stream -> {:ok, :ok} end,
        interceptor_state
      )

    assert_received({:setting_headers, %{"access-control-allow-origin" => @function_header_value}}, "Incorrect header when using function")

  end
end

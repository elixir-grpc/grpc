defmodule GRPC.Integration.ErrorDetailsTest do
  use GRPC.Integration.TestCase

  defmodule ErrorDetailsServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      case req.name do
        "OK" ->
          %Helloworld.HelloReply{message: "Hello, OK"}

        "INVALID_ARGUMENT" ->
          error_info = %Google.Rpc.ErrorInfo{
            reason: "INVALID_NAME",
            domain: "example.com",
            metadata: %{"field" => "name"}
          }

          detail = %Google.Protobuf.Any{
            type_url: "type.googleapis.com/google.rpc.ErrorInfo",
            value: Google.Rpc.ErrorInfo.encode(error_info)
          }

          raise GRPC.RPCError,
            status: GRPC.Status.invalid_argument(),
            message: "Invalid name provided",
            details: [detail]

        "MULTIPLE_DETAILS" ->
          error_info1 = %Google.Rpc.ErrorInfo{
            reason: "RATE_LIMIT_EXCEEDED",
            domain: "api.example.com",
            metadata: %{"quota" => "100"}
          }

          error_info2 = %Google.Rpc.ErrorInfo{
            reason: "RESOURCE_EXHAUSTED",
            domain: "api.example.com",
            metadata: %{"resource" => "cpu"}
          }

          details = [
            %Google.Protobuf.Any{
              type_url: "type.googleapis.com/google.rpc.ErrorInfo",
              value: Google.Rpc.ErrorInfo.encode(error_info1)
            },
            %Google.Protobuf.Any{
              type_url: "type.googleapis.com/google.rpc.ErrorInfo",
              value: Google.Rpc.ErrorInfo.encode(error_info2)
            }
          ]

          raise GRPC.RPCError,
            status: GRPC.Status.resource_exhausted(),
            message: "Multiple issues found",
            details: details

        _ ->
          raise GRPC.RPCError,
            status: GRPC.Status.unknown(),
            message: "Unknown error without details"
      end
    end
  end

  test "server returns error with single detail and client receives it" do
    run_server(ErrorDetailsServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "INVALID_ARGUMENT"}

      {:error, error} = Helloworld.Greeter.Stub.say_hello(channel, req)

      assert error.status == GRPC.Status.invalid_argument()
      assert error.message == "Invalid name provided"
      assert length(error.details) == 1

      [detail] = error.details
      assert detail.type_url == "type.googleapis.com/google.rpc.ErrorInfo"

      error_info = Google.Rpc.ErrorInfo.decode(detail.value)
      assert error_info.reason == "INVALID_NAME"
      assert error_info.domain == "example.com"
      assert error_info.metadata == %{"field" => "name"}
    end)
  end

  test "server returns error with multiple details and client receives them" do
    run_server(ErrorDetailsServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "MULTIPLE_DETAILS"}

      {:error, error} = Helloworld.Greeter.Stub.say_hello(channel, req)

      assert error.status == GRPC.Status.resource_exhausted()
      assert error.message == "Multiple issues found"
      assert length(error.details) == 2

      [detail1, detail2] = error.details

      error_info1 = Google.Rpc.ErrorInfo.decode(detail1.value)
      assert error_info1.reason == "RATE_LIMIT_EXCEEDED"
      assert error_info1.domain == "api.example.com"
      assert error_info1.metadata == %{"quota" => "100"}

      error_info2 = Google.Rpc.ErrorInfo.decode(detail2.value)
      assert error_info2.reason == "RESOURCE_EXHAUSTED"
      assert error_info2.domain == "api.example.com"
      assert error_info2.metadata == %{"resource" => "cpu"}
    end)
  end

  test "server returns error without details" do
    run_server(ErrorDetailsServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "UNKNOWN"}

      {:error, error} = Helloworld.Greeter.Stub.say_hello(channel, req)

      assert error.status == GRPC.Status.unknown()
      assert error.message == "Unknown error without details"
      assert is_nil(error.details)
    end)
  end

  test "server returns success (no error)" do
    run_server(ErrorDetailsServer, fn port ->
      {:ok, channel} = GRPC.Stub.connect("localhost:#{port}")
      req = %Helloworld.HelloRequest{name: "OK"}

      {:ok, reply} = Helloworld.Greeter.Stub.say_hello(channel, req)

      assert reply.message == "Hello, OK"
    end)
  end
end

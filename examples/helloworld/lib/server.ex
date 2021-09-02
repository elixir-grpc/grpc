defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(%{name: "quota_failure:" <> name}, _stream) do
    detail =
      Google.Rpc.QuotaFailure.new(
        violations: [%{subject: "name:#{name}", description: "Limit one greeting per person"}]
      )

    raise GRPC.RPCError, status: GRPC.Status.resource_exhausted(), details: [detail]
  end

  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end

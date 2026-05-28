defmodule GRPC do
  @moduledoc """
  GRPC is a fully featured Elixir implementation of the gRPC protocol (grpc.io),
  enabling efficient communication between services through a unified and
  stream-oriented API. It supports all RPC types, friendly error handling, TLS,
  interceptors, reflection, and optional HTTP transcoding.

  Suitable for both server and client development in pure Elixir, enabling
  scalable, efficient and type-safe distributed systems.

  ## Main features:

    * Unary, Server Streaming, Client Streaming, Bi-directional Streaming RPCs;
    * Streaming-first API for every call;
    * Interceptors;
    * Error handling with predictable propagation;
    * TLS authentication and message compression;
    * Connection load balancing strategies (Round Robin, Pick First);
    * gRPC Reflection;
    * HTTP Transcoding for REST ↔ gRPC compatibility;

  ## Installation:

      def deps do
        [
          {:grpc, "~> 0.11"},
          {:protobuf, "~> 0.14"},
          {:grpc_reflection, "~> 0.2"}
        ]
      end

  ## Protobuf code generation:

      protoc --elixir_out=plugins=grpc:./lib -I./priv/protos helloworld.proto

  ## Basic Server Example

      defmodule MyApp.Greeter.Server do
        use GRPC.Server, service: MyApp.Greeter.Service
        alias MyApp.{HelloRequest, HelloReply}

        def say_hello(request, stream) do
          request
          |> GRPC.Stream.unary(materializer: stream)
          |> GRPC.Stream.map(fn %HelloRequest{name: name} ->
            %HelloReply{message: "Hello"}
          end)
          |> GRPC.Stream.run()
        end
      end

      defmodule MyApp.Endpoint do
        use GRPC.Endpoint
        run MyApp.Greeter.Server
      end

      children = [
        {GRPC.Server.Supervisor, endpoint: MyApp.Endpoint, port: 50051}
      ]

  ## Server-side streaming:

      def say_hi_stream(request, stream) do
        Stream.repeatedly(fn ->
          %HelloReply{message: "Hi!"}
        end)
        |> Stream.take(5)
        |> GRPC.Stream.from()
        |> GRPC.Stream.run_with(stream)
      end

  ## Bidirectional streaming:

      def chat(request_enum, stream) do
        GRPC.Stream.from(request_enum)
        |> GRPC.Stream.map(fn req ->
          %HelloReply{message: "I'm the Server ;)"}
        end)
        |> GRPC.Stream.run_with(stream)
      end

  See `GRPC.Stream` for more Server examples.

  ## Basic Client Example

      {:ok, _} = GRPC.Client.Supervisor.start_link()
      {:ok, channel} = GRPC.Stub.connect("localhost:50051")

      req = MyApp.HelloRequest.new(name: "Elixir")
      {:ok, reply} = MyApp.Greeter.Stub.say_hello(channel, req)

  See `GRPC.Stub` for more Client examples.

  ## HTTP Transcoding (optional)

  Enable REST-to-gRPC mapping:

      use GRPC.Server,
        service: MyApp.Greeter.Service,
        http_transcode: true

  Useful when interacting with gRPC from browsers or REST clients.

  ## CORS Support (optional)

      defmodule MyApp.Endpoint do
        use GRPC.Endpoint
        intercept GRPC.Server.Interceptors.CORS, allow_origin: "*"
        run MyApp.Greeter.Server
      end

  """
  @version GRPC.Mixfile.project()[:version]

  @doc """
  Returns version of this project.
  """
  def version, do: @version

  @doc false
  def user_agent, do: "grpc-elixir/#{version()}"
end

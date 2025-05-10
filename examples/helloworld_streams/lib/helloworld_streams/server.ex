defmodule HelloworldStreams.Server do
  @moduledoc """
  gRPC service for streaming data.
  """
  use GRPC.Server, service: Stream.EchoServer.Service

  alias HelloworldStreams.Utils.Transformer
  alias GRPC.Stream, as: GRPCStream

  alias Stream.HelloRequest
  alias Stream.HelloReply

  @spec say_unary_hello(HelloRequest.t(), GRPC.Server.Stream.t()) :: any()
  def say_unary_hello(request, materializer) do
    GRPCStream.single(request)
    |> GRPCStream.ask(Transformer)
    |> GRPCStream.map(fn %HelloReply{} = reply ->
      %HelloReply{message: "[Reply] #{reply.message}"}
    end)
    |> GRPCStream.run_with(materializer)
  end

  @spec say_server_hello(HelloRequest.t(), GRPC.Server.Stream.t()) :: any()
  def say_server_hello(request, materializer) do
    GRPCStream.single(request, propagate_context: true, materializer: materializer)
    # Create new output stream from the request and drop the input
    |> GRPCStream.via(fn _metadata, %HelloRequest{} = msg ->
      create_output_stream(msg)
      |> GRPCStream.from()
    end)
    |> GRPCStream.run()
  end

  defp create_output_stream(msg) do
    Stream.repeatedly(fn ->
      index = :rand.uniform(10)
      %HelloReply{message: "[#{index}] I'm the Server for #{msg.name}"}
    end)
    |> Stream.take(10)
    |> Enum.to_list()
  end

  @spec say_bid_stream_hello(Enumerable.t(), GRPC.Server.Stream.t()) :: any()
  def say_bid_stream_hello(request, materializer) do
    # simulate a infinite stream of data
    # this is a simple example, in a real world application
    # you would probably use a GenStage or similar
    # to handle the stream of data
    output_stream =
      Stream.repeatedly(fn ->
        index = :rand.uniform(10)
        %HelloReply{message: "[#{index}] I'm the Server ;)"}
      end)

    GRPCStream.from(request, join_with: output_stream)
    |> GRPCStream.map(fn
      %HelloRequest{} = hello ->
        %HelloReply{message: "Welcome #{hello.name}"}

      output_item ->
        output_item
    end)
    |> GRPCStream.run_with(materializer)
  end
end

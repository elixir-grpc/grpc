defmodule GRPC.Reflection.Service do
  @moduledoc """
  This module implement gRPC Reflection Service.
  """
  use GRPC.Server, service: Grpc.Reflection.V1alpha.ServerReflection.Service
  require Logger

  alias GRPC.Reflection
  alias Grpc.Reflection.V1alpha.{ServerReflectionRequest, ServerReflectionResponse, ErrorResponse}

  alias GRPC.Server

  @spec server_reflection_info(ServerReflectionRequest.t(), GRPC.Server.Stream.t()) ::
          ServerReflectionResponse.t()
  def server_reflection_info(request, stream) do
    Enum.each(request, fn message ->
      Logger.debug("Received reflection request: #{inspect(message)}")

      case message.message_request do
        {:list_services, _} ->
          Server.send_reply(stream, Reflection.list_services())

        {:file_containing_symbol, _} ->
          symbol = elem(message.message_request, 1)
          Server.send_reply(stream, Reflection.find_by_symbol(symbol))

        {:file_by_filename, _} ->
          filename = elem(message.message_request, 1)
          Server.send_reply(stream, Reflection.find_by_filename(filename))

        _ ->
          Logger.warn("This Reflection Operation is not supported")

          response =
            ServerReflectionResponse.new(
              message_response:
                {:error_response,
                 ErrorResponse.new(error_code: 13, error_message: "Operation not supported")}
            )

          Server.send_reply(stream, response)
      end
    end)
  end
end

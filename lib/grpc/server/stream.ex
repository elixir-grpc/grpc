defmodule GRPC.Server.Stream do
  @moduledoc """
  A struct as an argument that servers get in rpc function definitions and use to handle headers,
  send streaming replies.

  Notice that you MUST use new stream returned by `GRPC.Server` as an argument to invoke next
  functions defined by `GRPC.Server`.

  ## Fields

    * `:server`            - user defined gRPC server module
    * `:adapter`           - a server adapter module, like `GRPC.Server.Adapters.Cowboy`
    * `request_mod`        - the request module, or nil for untyped protocols
    * `response_mod`       - the response module, or nil for untyped protocols
    * `:codec`             - the codec
    * `:payload`           - the payload needed by the adapter
    * `:local`             - local data initialized by user
  """

  @type t :: %__MODULE__{
          server: atom(),
          service_name: String.t(),
          method_name: String.t(),
          grpc_type: atom(),
          endpoint: atom(),
          rpc: tuple(),
          request_mod: atom(),
          request_id: String.t() | nil,
          response_mod: atom(),
          codec: atom(),
          payload: any(),
          adapter: atom(),
          local: any(),
          # compressor mainly is used in client decompressing, responses compressing should be set by
          # `GRPC.Server.set_compressor`
          compressor: module() | nil,
          # For http transcoding
          http_method: GRPC.Server.Router.http_method(),
          http_transcode: boolean(),
          __interface__: map()
        }

  defstruct server: nil,
            service_name: nil,
            method_name: nil,
            grpc_type: nil,
            endpoint: nil,
            rpc: nil,
            request_mod: nil,
            request_id: nil,
            response_mod: nil,
            codec: GRPC.Codec.Proto,
            payload: nil,
            adapter: nil,
            local: nil,
            compressor: nil,
            http_method: :post,
            http_transcode: false,
            __interface__: %{send_reply: &__MODULE__.send_reply/3}

  def send_reply(
        %{grpc_type: :server_stream, codec: codec, http_transcode: true, rpc: rpc} = stream,
        reply,
        opts
      ) do
    rule = GRPC.Service.rpc_options(rpc, :http) || %{value: %{}}
    response = GRPC.Server.Transcode.map_response_body(rule.value, reply)

    do_send_reply(stream, [codec.encode(response), "\n"], opts)
  end

  def send_reply(%{codec: codec, http_transcode: true, rpc: rpc} = stream, reply, opts) do
    rule = GRPC.Service.rpc_options(rpc, :http) || %{value: %{}}
    response = GRPC.Server.Transcode.map_response_body(rule.value, reply)

    do_send_reply(stream, codec.encode(response), opts)
  end

  def send_reply(%{codec: codec} = stream, reply, opts) do
    do_send_reply(stream, codec.encode(reply), opts)
  end

  defp do_send_reply(
         %{adapter: adapter, codec: codec, http_transcode: http_transcode} = stream,
         data,
         opts
       ) do
    opts =
      opts
      |> Keyword.put(:codec, codec)
      |> Keyword.put(:http_transcode, http_transcode)

    adapter.send_reply(stream.payload, data, opts)

    stream
  end
end

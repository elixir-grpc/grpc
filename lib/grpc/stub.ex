defmodule GRPC.Stub do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      Enum.each service_mod.__rpc_calls__, fn ({name, {_, req_stream}, {_, res_stream}} = rpc) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        if req_stream do
          def unquote(String.to_atom(func_name))(channel, opts \\ []) do
            GRPC.Stub.call(unquote(service_mod), unquote(Macro.escape(rpc)), unquote(path), channel, nil, opts)
          end
        else
          def unquote(String.to_atom(func_name))(channel, request, opts \\ []) do
            GRPC.Stub.call(unquote(service_mod), unquote(Macro.escape(rpc)), unquote(path), channel, request, opts)
          end
        end
      end
    end
  end

  def call(service_mod, rpc, path, channel, request, opts) do
    {_, {req_mod, req_stream}, {res_mod, res_stream}} = rpc
    marshal = fn(req) -> service_mod.marshal(req_mod, req) end
    unmarshal = fn(res) -> service_mod.unmarshal(res_mod, res) end
    service = %{marshal: marshal, unmarshal: unmarshal, path: path, req_stream: req_stream, res_stream: res_stream}
    stream = %GRPC.Stream{service: service}
    send_request(req_stream, res_stream, stream, channel, request, opts)
  end

  defp send_request(false, false, %{service: service} = stream, channel, request, opts) do
    %{marshal: marshal, unmarshal: unmarshal, path: path} = service
    message = marshal.(request)
    {:ok, response} = GRPC.Call.unary(channel, path, message, opts)
    parse_unary_response(response, unmarshal)
  end
  defp send_request(false, true, %{service: service} = stream, channel, request, opts) do
    %{marshal: marshal, unmarshal: unmarshal, path: path} = service
    message = marshal.(request)
    {:ok, stream_id} = GRPC.Call.send_request(channel, path, message, opts)
    response_stream(channel, stream_id, unmarshal)
  end
  defp send_request(true, false, %{service: service} = stream, channel, _, opts) do
    %{path: path} = service
    {:ok, stream_id} = GRPC.Call.send_header(channel, path, opts)
    %{stream | state: %{channel: channel, stream_id: stream_id}}
  end
  defp send_request(true, true, %{service: service} = stream, channel, _, opts) do
    %{path: path} = service
    {:ok, stream_id} = GRPC.Call.send_header(channel, path, opts)
    %{stream | state: %{channel: channel, stream_id: stream_id}}
  end

  def stream_send(%{service: service, state: %{stream_id: stream_id, channel: channel}}, request, opts \\ []) do
    message = service[:marshal].(request)
    send_end_stream = Keyword.get(opts, :end_stream, false)
    GRPC.Call.send_body(channel, stream_id, message, send_end_stream: send_end_stream)
    :ok
  end

  def recv(stream, opts \\ [])
  def recv(%{service: %{res_stream: true, unmarshal: unmarshal}, state: %{stream_id: stream_id, channel: channel}}, opts) do
    response_stream(channel, stream_id, unmarshal)
  end
  def recv(%{service: service, state: %{stream_id: stream_id, channel: channel}}, opts) do
    {:ok, response} = GRPC.Call.recv_end(channel, stream_id, opts)
    parse_unary_response(response, service[:unmarshal])
  end

  defp parse_unary_response({_headers, data_list}, unmarshal) do
    data_list
    |> Enum.map(&GRPC.Message.from_data/1)
    |> Enum.map(& unmarshal.(&1))
    |> List.first
  end

  defp response_stream(channel, stream_id, unmarshal) do
    Stream.unfold([], fn acc ->
      case GRPC.Call.recv(channel, stream_id) do
        {:data, data} ->
          reply = data
          |> GRPC.Message.from_data
          |> unmarshal.()
          {reply, [acc] ++ reply}
        {:end_stream, _resp} ->
          nil
        other ->
          IO.inspect other
      end
    end)
  end
end

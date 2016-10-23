defmodule GRPC.Stub do
  alias GRPC.Channel

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
    stream = %GRPC.Client.Stream{marshal: marshal, unmarshal: unmarshal, path: path,
              req_stream: req_stream, res_stream: res_stream, channel: channel}
    send_request(req_stream, res_stream, stream, request, opts)
  end

  defp send_request(false, false, %{marshal: marshal, unmarshal: unmarshal} = stream, request, opts) do
    message = marshal.(request)
    {:ok, response} = Channel.unary(stream, message, opts)
    parse_unary_response(response, unmarshal)
  end
  defp send_request(false, true, %{marshal: marshal} = stream, request, opts) do
    message = marshal.(request)
    {:ok, new_stream} = Channel.send_request(stream, message, opts)
    response_stream(new_stream, opts)
  end
  defp send_request(true, false, stream, _, opts) do
    {:ok, stream} = Channel.send_header(stream, opts)
    stream
  end
  defp send_request(true, true, stream, _, opts) do
    {:ok, stream} = Channel.send_header(stream, opts)
    stream
  end

  def stream_send(%{marshal: marshal} = stream, request, opts \\ []) do
    message = marshal.(request)
    send_end_stream = Keyword.get(opts, :end_stream, false)
    Channel.send_body(stream, message, send_end_stream: send_end_stream)
    :ok
  end

  def recv(stream, opts \\ [])
  def recv(%{res_stream: true} = stream, opts) do
    response_stream(stream, opts)
  end
  def recv(%{unmarshal: unmarshal} = stream, opts) do
    {:ok, response} = Channel.recv_end(stream, opts)
    parse_unary_response(response, unmarshal)
  end

  defp parse_unary_response({_headers, data_list}, unmarshal) do
    data_list
    |> Enum.map(&GRPC.Message.from_data/1)
    |> Enum.map(& unmarshal.(&1))
    |> List.first
  end

  defp response_stream(%{unmarshal: unmarshal} = stream, opts) do
    Stream.unfold([], fn acc ->
      case Channel.recv(stream, opts) do
        {:data, data} ->
          reply = data
          |> GRPC.Message.from_data
          |> unmarshal.()
          {reply, [acc] ++ reply}
        {:end_stream, _resp} ->
          nil
        other ->
          other
      end
    end)
  end
end

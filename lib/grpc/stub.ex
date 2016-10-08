defmodule GRPC.Stub do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      Enum.each service_mod.__rpc_calls__, fn ({name, _, _} = rpc) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def unquote(String.to_atom(func_name))(channel, request, opts \\ []) do
          GRPC.Stub.call(unquote(service_mod), unquote(Macro.escape(rpc)), unquote(path), channel, request, opts)
        end
      end
    end
  end

  def call(service_mod, rpc, path, channel, request, opts \\ []) do
    {_, {req_mod, req_stream}, {res_mod, res_stream}} = rpc
    marshal = fn(req) -> service_mod.marshal(req_mod, req) end
    unmarshal = fn(res) -> service_mod.unmarshal(res_mod, res) end
    cond do
      !req_stream && !res_stream ->
        [reply] = unary_call(path, marshal, unmarshal, channel, request, opts)
        reply
      !req_stream && res_stream ->
        server_stream_call(path, marshal, unmarshal, channel, request, opts)
    end
  end

  def unary_call(path, marshal, unmarshal, channel, request, opts \\ []) do
    message = marshal.(request)
    {:ok, {_headers, data_list}} = GRPC.Call.unary(channel, path, message, opts)
    data_list
    |> Enum.map(&GRPC.Message.from_data/1)
    |> Enum.map(& unmarshal.(&1))
  end

  def server_stream_call(path, marshal, unmarshal, channel, request, opts \\ []) do
    message = marshal.(request)
    {:ok, stream_id} = GRPC.Call.send_request(channel, path, message, opts)
    Stream.unfold([], fn acc ->
      case GRPC.Call.recv(channel, stream_id) do
        {:data, data} ->
          reply = data
          |> GRPC.Message.from_data
          |> unmarshal.()
          {reply, [acc] ++ reply}
        {:end_stream, _resp} ->
          nil
      end
    end)
  end
end

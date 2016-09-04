defmodule GRPC.Stub do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      marshal = service_mod.__meta__(:marshal_function)
      unmarshal = service_mod.__meta__(:unmarshal_function)
      Enum.each service_mod.__rpc_calls__, fn ({name, request_mod, reply_mod}) ->
        func_name = name |> to_string |> Macro.underscore
        route = "/#{service_name}/#{name}"
        def unquote(String.to_atom(func_name))(channel, request, metadata \\ %{}) do
          marshal_func = fn(req) -> apply(unquote(request_mod), unquote(marshal), [req]) end
          unmarshal_func = fn(res) -> apply(unquote(reply_mod), unquote(unmarshal), [res]) end
          GRPC.Stub.unary_call(channel, unquote(route), request, marshal_func, unmarshal_func, metadata)
        end
      end
    end
  end

  def unary_call(channel, route, request, marshal_func, unmarshal_func, metadata, _options \\ []) do
    cq = GRPC.Core.CompletionQueue.create
    deadline = :os.system_time(:seconds) + 300
    call = GRPC.Core.Call.create(channel, nil, nil, cq, String.to_charlist(route), nil, deadline)

    message = marshal_func.(request)
    result = GRPC.Call.unary(call, cq, message, metadata)
    Map.put(result, :message, unmarshal_func.(result[:message]))
  end
end

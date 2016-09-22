defmodule GRPC.Stub do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      marshal = service_mod.__meta__(:marshal_function)
      unmarshal = service_mod.__meta__(:unmarshal_function)
      Enum.each service_mod.__rpc_calls__, fn ({name, request_mod, reply_mod}) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def unquote(String.to_atom(func_name))(channel, request, opts \\ []) do
          marshal_func = fn(req) -> apply(unquote(request_mod), unquote(marshal), [req]) end
          unmarshal_func = fn(res) -> apply(unquote(reply_mod), unquote(unmarshal), [res]) end
          GRPC.Stub.unary_call(channel, unquote(path), request, marshal_func, unmarshal_func, opts)
        end
      end
    end
  end

  def unary_call(channel, path, request, marshal_func, unmarshal_func, opts \\ []) do
    message = marshal_func.(request)
    {:ok, {_headers, [data]}} = GRPC.Call.unary(channel, path, message, opts)
    message = GRPC.Message.from_data(data)
    unmarshal_func.(message)
  end
end

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

  def call(service_mod, {name, {req_mod, _}, {res_mod, _}} = rpc, path, channel, request, opts \\ []) do
    marshal = fn(req) -> service_mod.marshal(req_mod, req) end
    unmarshal = fn(res) -> service_mod.unmarshal(res_mod, res) end
    unary_call(path, marshal, unmarshal, channel, request, opts)
  end

  def unary_call(path, marshal_func, unmarshal_func, channel, request, opts \\ []) do
    message = marshal_func.(request)
    {:ok, {_headers, [data]}} = GRPC.Call.unary(channel, path, message, opts)
    message = GRPC.Message.from_data(data)
    unmarshal_func.(message)
  end
end

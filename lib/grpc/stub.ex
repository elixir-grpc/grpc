defmodule GRPC.Stub do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      Enum.each service_mod.__rpc_calls__, fn ({name, request_mod, reply_mod}) ->
        func_name = name |> to_string |> Macro.underscore
        def unquote(String.to_atom(func_name))(channel, request) do
          apply(unquote(request_mod), unquote(service_mod.__meta__(:marshal_function)), [request])
        end
      end
    end
  end
end

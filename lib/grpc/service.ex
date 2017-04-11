defmodule GRPC.Service do
  @moduledoc """
    GRPC.Service 
  """

  defmacro __using__(opts) do
    quote do
      import GRPC.Service, only: [rpc: 3]
      @service_opts unquote(opts)

      Module.register_attribute(__MODULE__, :rpc_calls, accumulate: true)
      @before_compile GRPC.Service
    end
  end

  defmacro __before_compile__(env) do
    rpc_calls = Module.get_attribute(env.module, :rpc_calls)
    service_opts = Module.get_attribute(env.module, :service_opts)

    Enum.each [:name, :marshal_function, :unmarshal_function], fn key ->
      unless service_opts[key], do: raise "#{key} has to be provided"
    end

    quote do
      def __rpc_calls__, do: unquote(Macro.escape(rpc_calls))
      def __meta__(:name), do: unquote(service_opts[:name])
      def __meta__(:marshal_function), do: unquote(service_opts[:marshal_function])
      def __meta__(:unmarshal_function), do: unquote(service_opts[:unmarshal_function])
    end
  end

  defmacro rpc(name, request, reply) do
    quote do
      @rpc_calls {unquote(name), unquote(request), unquote(reply)}
    end
  end

end

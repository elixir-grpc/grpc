defmodule GRPC.Service do
  defmacro __using__(opts) do
    quote do
      import GRPC.Service, only: [rpc: 3, stream: 1]
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
      def __rpc_calls__, do: unquote(rpc_calls |> Macro.escape |> Enum.reverse)
      def __meta__(:name), do: unquote(service_opts[:name])
      def __meta__(:marshal_function), do: unquote(service_opts[:marshal_function])
      def __meta__(:unmarshal_function), do: unquote(service_opts[:unmarshal_function])
    end
  end

  defmacro rpc(name, request, reply) do
    quote do
      @rpc_calls {unquote(name), unquote(wrap_stream(request)), unquote(wrap_stream(reply))}
    end
  end

  def stream(param) do
    quote do: {unquote(param), true}
  end

  def wrap_stream({:stream, _, _} = param) do
    quote do: unquote(param)
  end
  def wrap_stream(param) do
    quote do: {unquote(param), false}
  end
end

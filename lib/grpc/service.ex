defmodule GRPC.Service do
  defmacro __using__(opts) do
    quote do
      import GRPC.Service, only: [rpc: 3, stream: 1]

      Module.register_attribute(__MODULE__, :rpc_calls, accumulate: true)
      @before_compile GRPC.Service

      def marshal(mod, message) do
        func = unquote(opts[:marshal]) || &GRPC.Message.Protobuf.encode/2
        func.(mod, message)
      end

      def unmarshal(mod, message) do
        func = unquote(opts[:unmarshal]) || &GRPC.Message.Protobuf.decode/2
        func.(mod, message)
      end
      def __meta__(:name), do: unquote(opts[:name])
    end
  end

  defmacro __before_compile__(env) do
    rpc_calls = Module.get_attribute(env.module, :rpc_calls)
    quote do
      def __rpc_calls__, do: unquote(rpc_calls |> Macro.escape |> Enum.reverse)
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

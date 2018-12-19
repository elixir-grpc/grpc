defmodule GRPC.Endpoint do
  @doc false
  defmacro __using__(_opts) do
    quote do
      import GRPC.Endpoint, only: [intercept: 1, intercept: 2, run: 1, run: 2]

      Module.register_attribute(__MODULE__, :interceptors, accumulate: true)
      Module.register_attribute(__MODULE__, :servers, accumulate: true)
      @before_compile GRPC.Endpoint
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    interceptors =
      Module.get_attribute(env.module, :interceptors)
      |> Macro.escape()
      |> Enum.reverse()
      |> init_interceptors()

    servers = Module.get_attribute(env.module, :servers)
    servers = Enum.map(servers, fn {ss, opts} -> {ss, parse_run_opts(opts, %{})} end)
    server_interceptors = server_interceptors(servers, %{})
    servers = parse_servers(servers)

    quote do
      def __meta__(:interceptors), do: unquote(interceptors)
      def __meta__(:servers), do: unquote(servers)
      def __meta__(:server_interceptors), do: unquote(Macro.escape(server_interceptors))
    end
  end

  defmacro intercept(name) do
    quote do
      @interceptors unquote(name)
    end
  end

  defmacro intercept(name, opts) do
    quote do
      @interceptors {unquote(name), unquote(opts)}
    end
  end

  defmacro run(servers, opts \\ []) do
    quote do
      @servers {unquote(servers), unquote(opts)}
    end
  end

  defp server_interceptors([], acc), do: acc

  defp server_interceptors([{servers, %{interceptors: interceptors}} | tail], acc0)
       when is_list(interceptors) do
    acc =
      Enum.reduce(List.wrap(servers), acc0, fn server, acc ->
        Map.put(acc, server, init_interceptors(interceptors))
      end)

    server_interceptors(tail, acc)
  end

  defp server_interceptors([_ | tail], acc) do
    server_interceptors(tail, acc)
  end

  defp parse_servers(servers) do
    servers
    |> Enum.map(fn {server, _} -> server end)
    |> List.flatten()
  end

  defp parse_run_opts([], acc), do: acc

  defp parse_run_opts([{:interceptors, interceptors} | tail], acc) do
    parse_run_opts(tail, Map.put(acc, :interceptors, interceptors))
  end

  defp parse_run_opts([{k, _} | _], _) do
    raise ArgumentError, message: "Unknown option for GRPC.Endpoint.run/2: #{k}"
  end

  def init_interceptors(interceptors) do
    Enum.map(interceptors, fn
      {interceptor, opts} ->
        {interceptor, interceptor.init(opts)}

      interceptor ->
        {interceptor, interceptor.init([])}
    end)
  end
end

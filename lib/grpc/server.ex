defmodule GRPC.Server do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      Enum.each service_mod.__rpc_calls__, fn ({name, _, _} = rpc) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def __call_rpc__(unquote(path), body, conn) do
          GRPC.Server.call(unquote(service_mod), conn, unquote(Macro.escape(rpc)), String.to_atom(unquote(func_name)), body)
        end
        def __call_rpc(_, _, conn), do: {:error, conn, "Error"}
      end
    end
  end

  def call(service_mod, %{server: server_mod} = conn,
           {_, {req_mod, req_stream}, {res_mod, res_stream}}, func_name, body) do
    marshal_func = fn(req) -> service_mod.marshal(res_mod, req) end
    unmarshal_func = fn(res) -> service_mod.unmarshal(req_mod, res) end
    conn = %{conn | marshal: marshal_func, unmarshal: unmarshal_func}
    request = unmarshal_func.(body)
    cond do
      !req_stream && !res_stream ->
        response = apply(server_mod, func_name, [request, conn])
        {:ok, conn, response}
      !req_stream && res_stream ->
        apply(server_mod, func_name, [request, conn])
        {:ok, conn}
    end
  end

  def stream_send(%{marshal: marshal, state: req}, response) do
    {:ok, data} = response |> marshal.() |> GRPC.Message.to_data(iolist: true)
    :cowboy_req.stream_body(data, :nofin, req)
  end

  def start(server, addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    start(server, host, port, opts)
  end
  def start(server, host, port, opts) when is_binary(port) do
    start(server, host, String.to_integer(port), opts)
  end
  def start(server, host, port, opts) when is_integer(port) do
    dispatch = :cowboy_router.compile([
      {host, [{:_, GRPC.Handler, {server, opts}}]}
    ])
    {:ok, _} = :cowboy.start_clear(server, 100,
      [port: port], %{:env => %{dispatch: dispatch}}
    )
    GRPC.ServerSup.start_link
  end

  def stop(server) do
    :cowboy.stop_listener(server)
  end
end

defmodule GRPC.ServerSup do
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    procs = []
    {:ok, {{:one_for_one, 1, 5}, procs}}
  end
end

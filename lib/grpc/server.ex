defmodule GRPC.Server do
  defmacro __using__(opts) do
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      Enum.each service_mod.__rpc_calls__, fn ({name, _, _} = rpc) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def __call_rpc__(unquote(path), stream) do
          GRPC.Server.call(unquote(service_mod), stream, unquote(Macro.escape(rpc)), String.to_atom(unquote(func_name)))
        end
        def __call_rpc(_, stream), do: {:error, stream, "Error"}
      end
    end
  end

  def call(service_mod, stream, {_, {req_mod, req_stream}, {res_mod, res_stream}} = _rpc, func_name) do
    marshal_func = fn(res) -> service_mod.marshal(res_mod, res) end
    unmarshal_func = fn(req) -> service_mod.unmarshal(req_mod, req) end
    stream = %{stream | marshal: marshal_func, unmarshal: unmarshal_func}

    handle_request(req_stream, res_stream, stream, func_name)
  end

  defp handle_request(false = req_stream, res_stream, %{unmarshal: unmarshal, adapter: adapter} = stream, func_name) do
    {:ok, data, stream} = adapter.read_body(stream)
    message = GRPC.Message.from_data(data)
    request = unmarshal.(message)
    handle_request(req_stream, res_stream, stream, func_name, request)
  end
  defp handle_request(true = req_stream, res_stream, %{unmarshal: unmarshal, adapter: adapter} = stream, func_name) do
    reading_stream = adapter.reading_stream(stream, fn (data) ->
      data
      |> GRPC.Message.from_data
      |> unmarshal.()
    end)
    handle_request(req_stream, res_stream, stream, func_name, reading_stream)
  end

  defp handle_request(false, false, %{server: server_mod} = stream, func_name, request) do
    response = apply(server_mod, func_name, [request, stream])
    {:ok, stream, response}
  end
  defp handle_request(false, true, %{server: server_mod} = stream, func_name, request) do
    apply(server_mod, func_name, [request, stream])
    {:ok, stream}
  end
  defp handle_request(true, false, %{server: server_mod} = stream, func_name, req_stream) do
    response = apply(server_mod, func_name, [req_stream, stream])
    {:ok, stream, response}
  end
  defp handle_request(true, true, %{server: server_mod} = stream, func_name, req_stream) do
    apply(server_mod, func_name, [req_stream, stream])
    {:ok, stream}
  end

  def start(server, addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    start(server, host, port, opts)
  end
  def start(server, host, port, opts) when is_binary(port) do
    start(server, host, String.to_integer(port), opts)
  end
  def start(server, host, port, opts) when is_integer(port) do
    adapter = Keyword.get(opts, :adapter, GRPC.Adapter.Cowboy)
    adapter.start(server, host, port, opts)
  end

  def stop(server, opts \\ []) do
    adapter = Keyword.get(opts, :adapter, GRPC.Adapter.Cowboy)
    adapter.stop(server)
  end

  def stream_send(%{adapter: adapter, marshal: marshal} = stream, response) do
    {:ok, data} = response |> marshal.() |> GRPC.Message.to_data(iolist: true)
    adapter.stream_send(stream, data)
  end
end

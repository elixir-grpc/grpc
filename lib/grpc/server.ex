defmodule GRPC.Server do
  @moduledoc """
    GRPC.Server 
  """

  defmacro __using__(opts) do
    # TODO: remove the duplicated code with stub
    quote bind_quoted: [service_mod: opts[:service]] do
      service_name = service_mod.__meta__(:name)
      marshal = service_mod.__meta__(:marshal_function)
      unmarshal = service_mod.__meta__(:unmarshal_function)
      Enum.each service_mod.__rpc_calls__, fn ({name, request_mod, reply_mod}) ->
        func_name = name |> to_string |> Macro.underscore
        path = "/#{service_name}/#{name}"
        def __call_rpc__(unquote(path), body) do
          marshal_func = fn(req) -> apply(unquote(reply_mod), unquote(marshal), [req]) end
          unmarshal_func = fn(res) -> apply(unquote(request_mod), unquote(unmarshal), [res]) end
          request = unmarshal_func.(body)
          response = apply(__MODULE__, String.to_atom(unquote(func_name)), [request])
          {:ok, marshal_func.(response)}
        end
        def __call_rpc(_, _), do: {:error, "Error"}
      end
    end
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
    {:ok, _} = :cowboy.start_clear(:http, 100,
      [port: port], %{:env => %{dispatch: dispatch}}
    )
    GRPC.ServerSup.start_link
  end
end

defmodule GRPC.ServerSup do
  @moduledoc """
    GRPC.ServerSup 
  """
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    procs = []
    {:ok, {{:one_for_one, 1, 5}, procs}}
  end
end

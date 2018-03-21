defmodule GRPC.Server.Supervisor do
  use Supervisor

  @moduledoc """
  A simple supervisor to start your servers.

  You can add it to your OTP tree as below. But to make the servers start, you have to config `grpc`

      defmodule Your.App do
        use Application

        def start(_type, _args) do
          import Supervisor.Spec

          children = [
            supervisor(GRPC.Server.Supervisor, [{Your.Server, 50051}])
          ]

          opts = [strategy: :one_for_one, name: __MODULE__]
          Supervisor.start_link(children, opts)
        end
      end

      # config.exs
      config :grpc, start_server: true
      or
      run `mix grpc.server` on local

  View `child_spec/3` for arguments.
  """

  @default_adapter GRPC.Adapter.Cowboy

  def start_link(servers) do
    Supervisor.start_link(__MODULE__, servers, name: __MODULE__)
  end

  @spec init({module | [module], integer}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({servers, port}) do
    init({servers, port, []})
  end

  @spec init({module | [module], integer, Keyword.t()}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({servers, port, opts}) do
    children =
      if Application.get_env(:grpc, :start_server, false) do
        [child_spec(servers, port, opts)]
      else
        []
      end

    supervise(children, strategy: :one_for_one)
  end

  @doc """
  Return a child_spec to start server.

  ## Options

    * `:cred` - a credential created by functions of `GRPC.Credential`,
      an insecure server will be created without this option
  """
  @spec child_spec(atom | [atom], integer, Keyword.t()) :: Supervisor.Spec.spec()
  def child_spec(servers, port, opts \\ []) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    servers = GRPC.Server.servers_to_map(servers)
    adapter.child_spec(servers, port, opts)
  end
end

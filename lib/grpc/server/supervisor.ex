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
            supervisor(GRPC.Server.Supervisor, [{Your.Endpoint, 50051(, opts)}])
          ]

          opts = [strategy: :one_for_one, name: __MODULE__]
          Supervisor.start_link(children, opts)
        end
      end

      # config.exs
      config :grpc, start_server: true
      or
      run `mix grpc.server` on local

  View `child_spec/3` for opts.
  """

  @default_adapter GRPC.Adapter.Cowboy
  require Logger

  def start_link(endpoint) do
    Supervisor.start_link(__MODULE__, endpoint)
  end

  @spec init({module | [module], integer}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({endpoint, port}) do
    init({endpoint, port, []})
  end

  @spec init({module | [module], integer, Keyword.t()}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({endpoint, port, opts}) do
    check_deps_version()

    children =
      if Application.get_env(:grpc, :start_server, false) do
        [child_spec(endpoint, port, opts)]
      else
        []
      end

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Return a child_spec to start server.

  ## Options

    * `:cred` - a credential created by functions of `GRPC.Credential`,
      an insecure server will be created without this option
  """
  @spec child_spec(atom | [atom], integer, Keyword.t()) :: Supervisor.Spec.spec()
  def child_spec(endpoint, port, opts \\ [])

  def child_spec(endpoint, port, opts) when is_atom(endpoint) do
    {endpoint, servers} =
      try do
        {endpoint, endpoint.__meta__(:servers)}
      rescue
        FunctionClauseError ->
          Logger.warn(
            "deprecated: servers as argument of GRPC.Server.Supervisor, please use GRPC.Endpoint"
          )

          {nil, endpoint}
      end

    adapter = Keyword.get(opts, :adapter, @default_adapter)
    servers = GRPC.Server.servers_to_map(servers)
    adapter.child_spec(endpoint, servers, port, opts)
  end

  def child_spec(servers, port, opts) when is_list(servers) do
    adapter = Keyword.get(opts, :adapter, @default_adapter)
    servers = GRPC.Server.servers_to_map(servers)
    adapter.child_spec(nil, servers, port, opts)
  end

  defp check_deps_version() do
    # cowlib
    case :application.get_key(:cowlib, :vsn) do
      {:ok, vsn} ->
        ver = to_string(vsn)

        unless Version.match?(ver, ">= 2.9.0") do
          Logger.warn("cowlib should be >= 2.9.0, it's #{ver} now. See grpc's README for details")
        end

      _ ->
        :ok
    end
  rescue
    _ ->
      :ok
  end
end

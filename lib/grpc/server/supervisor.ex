defmodule GRPC.Server.Supervisor do
  @moduledoc """
  A simple supervisor to start your servers.

  You can add it to your OTP tree as below. But to make the servers start, you have to config `grpc`

      defmodule Your.App do
        use Application

        def start(_type, _args) do
          children = [
            {GRPC.Server.Supervisor, {Your.Endpoint, 50051, opts}}
          ]

          opts = [strategy: :one_for_one, name: __MODULE__]
          Supervisor.start_link(children, opts)
        end
      end

      # config.exs
      config :grpc, start_server: true

  or

      run `mix grpc.server` on local

  or you can pass `start_server: true` as an option for the supervisor (as started above)
  """

  use Supervisor

  @default_adapter GRPC.Server.Adapters.Cowboy
  require Logger

  def start_link(endpoint) do
    Supervisor.start_link(__MODULE__, endpoint)
  end

  @spec init({module() | [module()], integer()}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({endpoint, port}) do
    init({endpoint, port, []})
  end

  @spec init({module() | [module()], integer(), Keyword.t()}) ::
          {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}} | :ignore
  def init({endpoint, port, opts}) do
    check_deps_version()

    # The code is repeated because it is concise and this way we can make it lazy
    children =
      cond do
        opts[:start_server] ->
          [child_spec(endpoint, port, opts)]

        not Keyword.has_key?(opts, :start_server) and Application.get_env(:grpc, :start_server) ->
          [child_spec(endpoint, port, opts)]

        :otherwise ->
          []
      end

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Return a child_spec to start server.

  ## Options

    * `:cred` - a credential created by functions of `GRPC.Credential`,
      an insecure server will be created without this option
    * `:start_server` - determines if the server will be started.
      If present, has more precedence then the `config :gprc, :start_server`
      config value (i.e. `start_server: false` will not start the server in any case).
  """
  @spec child_spec(atom() | [atom()], integer(), Keyword.t()) :: Supervisor.Spec.spec()
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

    adapter = Keyword.get(opts, :adapter) || @default_adapter
    servers = GRPC.Server.servers_to_map(servers)
    adapter.child_spec(endpoint, servers, port, opts)
  end

  def child_spec(servers, port, opts) when is_list(servers) do
    adapter = Keyword.get(opts, :adapter) || @default_adapter
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

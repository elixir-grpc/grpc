defmodule GRPC.Server.Supervisor do
  @moduledoc """
  A simple supervisor to start your servers.

  You can add it to your OTP tree as below.
  To start the server, you can pass `start_server: true` and an option

      defmodule Your.App do
        use Application

        def start(_type, _args) do
          children = [
            {GRPC.Server.Supervisor, endpoint: Your.Endpoint, port: 50051, start_server: true, adapter_opts: [ip: {0, 0, 0, 0}], ...}]

          Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
        end
      end

  """

  use Supervisor

  # TODO: remove this type after support Elixir 1.14 exclusively.
  @type sup_flags() :: %{
          strategy: Supervisor.strategy(),
          intensity: non_neg_integer(),
          period: pos_integer(),
          auto_shutdown: Supervisor.auto_shutdown()
        }

  @default_adapter GRPC.Server.Adapters.Cowboy
  require Logger

  def start_link(endpoint) do
    Supervisor.start_link(__MODULE__, endpoint)
  end

  @doc """
  ## Options

    * `:endpoint` - defines the endpoint module that will be started.
    * `:port` - the HTTP port for the endpoint.
    * `:servers` - the list of servers that will be be started.
    * `:adapter_opts` - options for the adapter.

  Either `:endpoint` or `:servers` must be present, but not both.
  """
  @spec init(tuple()) :: no_return
  @spec init(keyword()) :: {:ok, {sup_flags(), [Supervisor.child_spec()]}} | :ignore
  def init(opts)

  def init(opts) when is_tuple(opts) do
    raise ArgumentError,
          "passing a tuple as configuration for GRPC.Server.Supervisor is no longer supported. See the documentation for more information on how to configure."
  end

  def init(opts) when is_list(opts) do
    if not is_nil(Application.get_env(:grpc, :start_server)) do
      raise "the :start_server config key has been deprecated.\
      The currently supported way is to configure it\
      through the :start_server option for the GRPC.Server.Supervisor"
    end

    opts =
      case Keyword.validate(opts, [:endpoint, :servers, :start_server, :port, :adapter_opts]) do
        {:ok, _opts} ->
          opts

        {:error, _} ->
          raise ArgumentError,
                "just [:endpoint, :servers, :start_server, :port, :adapter_opts] are accepted as arguments, and any other keys for adapters should be passed as adapter_opts!"
      end

    case validate_cred(opts) do
      {:ok, _cred} -> :ok
      {:error, err} -> raise ArgumentError, err
    end

    endpoint_or_servers =
      case {opts[:endpoint], opts[:servers]} do
        {endpoint, servers}
        when (not is_nil(endpoint) and not is_nil(servers)) or
               (is_nil(endpoint) and is_nil(servers)) ->
          raise ArgumentError, "either :endpoint or :servers must be passed, but not both."

        {endpoint, nil} ->
          endpoint

        {nil, servers} when not is_list(servers) ->
          raise ArgumentError, "either :servers must be a list of modules"

        {nil, servers} when is_list(servers) ->
          servers
      end

    children =
      if opts[:start_server] do
        [child_spec(endpoint_or_servers, opts[:port], opts)]
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
    * `:start_server` - determines if the server will be started.
      If present, has more precedence then the `config :gprc, :start_server`
      config value (i.e. `start_server: false` will not start the server in any case).
  """
  @spec child_spec(endpoint_or_servers :: atom() | [atom()], port :: integer, opts :: keyword()) ::
          Supervisor.Spec.spec()
  def child_spec(endpoint_or_servers, port, opts \\ [])

  def child_spec(endpoint, port, opts) when is_atom(endpoint) do
    {endpoint, servers} =
      try do
        {endpoint, endpoint.__meta__(:servers)}
      rescue
        FunctionClauseError ->
          Logger.warning(
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

  defp validate_cred(opts) do
    with cred <- Kernel.get_in(opts, [:adapter_opts, :cred]),
         true <- cred == nil or (is_map(cred) and is_list(Map.get(cred, :ssl))) do
      {:ok, cred}
    else
      _ ->
        {:error,
         "the :cred option must be a map with an :ssl key containing a list of SSL options"}
    end
  end
end

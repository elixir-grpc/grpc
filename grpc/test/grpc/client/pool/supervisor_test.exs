defmodule GRPC.Client.Pool.SupervisorTest do
  use ExUnit.Case

  alias GRPC.Client.Pool.Config

  @subject GRPC.Client.Pool.Supervisor

  describe "start_link/1" do
    test "supervisor is started as well as its children" do
      {:ok, supervisor_pid} = start_supervised({@subject, make_config()})

      [
        {_, hc_supervisor_pid, :supervisor, [GRPC.Client.Pool.HealthCheck.DynamicSupervisor]},
        {_, server_pid, :worker, [GRPC.Client.Pool.Server]}
      ] =
        supervisor_pid
        |> Supervisor.which_children()
        |> Enum.sort_by(fn {_id, _pid, _type, [module]} -> module end)

      assert is_pid(server_pid)
      assert is_pid(hc_supervisor_pid)
    end
  end

  defp make_config do
    %Config{
      pool_ref: make_ref(),
      channel: %GRPC.Channel{
        host: "localhost",
        port: 1337,
        scheme: "http",
        adapter: GRPC.Client.Adapters.Gun,
        codec: GRPC.Codec.Proto,
        interceptors: [],
        compressor: nil,
        accepted_compressors: [],
        headers: []
      },
      pool_size: 1,
      max_pool_overflow: 20,
      max_client_streams_per_connection: 100,
      adapter_opts: []
    }
  end
end

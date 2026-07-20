defmodule GRPC.Client.Pool.HealthCheck.DynamicSupervisorTest do
  use ExUnit.Case

  alias GRPC.Client.Pool.Config

  @subject GRPC.Client.Pool.HealthCheck.DynamicSupervisor

  describe "start/3" do
    setup do
      config = make_config()
      supervisor_pid = start_supervised!({GRPC.Client.Pool.Supervisor, config})

      [
        {_, hc_supervisor_pid, :supervisor, [GRPC.Client.Pool.HealthCheck.DynamicSupervisor]},
        {_, _server_pid, :worker, [GRPC.Client.Pool.Server]}
      ] =
        supervisor_pid
        |> Supervisor.which_children()
        |> Enum.sort_by(fn {_id, _pid, _type, [module]} -> module end)

      %{hc_dynamic_supervisor_pid: hc_supervisor_pid, pool_ref: config.pool_ref, id: make_ref()}
    end

    test "successfully starts health-check server",
         %{
           hc_dynamic_supervisor_pid: hc_dynamic_supervisor_pid,
           pool_ref: pool_ref,
           id: channel_id
         } do
      loop_pid = spawn_infinite_process()

      assert {:ok, pid} = @subject.start(channel_id, loop_pid, pool_ref)

      assert [{:undefined, ^pid, :worker, [GRPC.Client.Pool.HealthCheck.Server]}] =
               hc_dynamic_supervisor_pid
               |> DynamicSupervisor.which_children()
               |> Enum.sort_by(fn {service_name, _, _, _} -> service_name end)
    end

    test "once process exits health-check server stops with reason normal and does not get restarted",
         %{pool_ref: pool_ref, id: id} do
      loop_pid = spawn_infinite_process()
      Process.monitor(loop_pid)

      opts = %{channel_id: id, conn_pid: loop_pid, pool_ref: pool_ref}
      pid = start_supervised!({GRPC.Client.Pool.HealthCheck.Server, opts})
      Process.monitor(pid)

      assert Process.alive?(pid)
      Process.exit(loop_pid, :kill)

      assert_receive {:DOWN, _ref, :process, ^loop_pid, _reason}
      assert_receive {:DOWN, _ref, :process, ^pid, _reason}

      refute Process.alive?(pid)
    end
  end

  defp spawn_infinite_process do
    spawn(fn ->
      receive do
        _ -> :ok
      end
    end)
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

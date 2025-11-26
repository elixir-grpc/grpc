defmodule GRPC.Server.SupervisorTest do
  use ExUnit.Case, async: false

  alias GRPC.Server.Supervisor

  defmodule MockEndpoint do
    def __meta__(_), do: [FeatureServer]
  end

  describe "init/1" do
    test "does not start children if opts sets false" do
      assert {:ok, {%{strategy: :one_for_one}, []}} =
               Supervisor.init(endpoint: MockEndpoint, port: 1234, start_server: false)
    end

    test "fails if a tuple is passed" do
      assert_raise ArgumentError,
                   "passing a tuple as configuration for GRPC.Server.Supervisor is no longer supported. See the documentation for more information on how to configure.",
                   fn ->
                     Supervisor.init({MockEndpoint, 1234})
                   end

      assert_raise ArgumentError,
                   "passing a tuple as configuration for GRPC.Server.Supervisor is no longer supported. See the documentation for more information on how to configure.",
                   fn ->
                     Supervisor.init({MockEndpoint, 1234, start_server: true})
                   end
    end

    test "starts children if opts sets true" do
      endpoint_str = "#{Macro.to_string(MockEndpoint)}"

      # assert {:ok,
      #         {%{strategy: :one_for_one},
      #          [
      #            %{
      #              id: {:ranch_listener_sup, ^endpoint_str},
      #              start: _,
      #              type: :supervisor
      #            }
      #          ]}} = Supervisor.init(endpoint: MockEndpoint, port: 1234, start_server: true)

      assert {
               :ok,
               {
                 %{strategy: :one_for_one, auto_shutdown: :never, intensity: 3, period: 5},
                 [
                   %{
                     id: {:ranch_embedded_sup, ^endpoint_str},
                     start:
                       {:ranch_embedded_sup, :start_link,
                        [
                          ^endpoint_str,
                          :ranch_tcp,
                          %{
                            num_acceptors: 20,
                            socket_opts: [port: 1234],
                            connection_type: :supervisor,
                            max_connections: 16384
                          },
                          :cowboy_clear,
                          %{
                            env: %{
                              dispatch:
                                {:persistent_term,
                                 GRPC.Server.SupervisorTest.MockEndpoint.Dispatch}
                            },
                            idle_timeout: :infinity,
                            inactivity_timeout: :infinity,
                            max_received_frame_rate: {10_000_000, 10000},
                            max_reset_stream_rate: {10000, 10000},
                            settings_timeout: :infinity,
                            stream_handlers: [:grpc_stream_h]
                          }
                        ]},
                     type: :supervisor
                   }
                 ]
               }
             } = Supervisor.init(endpoint: MockEndpoint, port: 1234, start_server: true)
    end
  end
end

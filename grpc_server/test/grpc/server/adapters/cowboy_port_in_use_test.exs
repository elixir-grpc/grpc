defmodule GRPC.Server.Adapters.CowboyPortInUseTest do
  # A listen failure gives a message that names the endpoint, the port and the reason,
  # and the original reason stays in the error term.
  # See https://github.com/elixir-grpc/grpc/issues/330
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  defmodule Endpoint do
    use GRPC.Endpoint
    run(FeatureServer)
  end

  setup do
    # Supervisor.start_link/2 links the caller to a supervisor that fails to start
    Process.flag(:trap_exit, true)

    # The OS picks the port, so the test never clashes with a server on a fixed port
    {:ok, listen_socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(listen_socket)
    on_exit(fn -> :gen_tcp.close(listen_socket) end)

    %{port: port}
  end

  describe "when the port is in use" do
    test "the supervisor logs the port and keeps :eaddrinuse in the error", %{port: port} do
      log =
        capture_log(fn ->
          assert {:error, reason} =
                   GRPC.Server.Supervisor.start_link(
                     endpoint: Endpoint,
                     port: port,
                     start_server: true
                   )

          assert contains?(reason, :eaddrinuse)
          assert {:listen_failed, message, _} = find_listen_failed(reason)
          assert message =~ "port #{port} is already in use (eaddrinuse)"
          assert message =~ inspect(Endpoint)
        end)

      assert log =~ "Could not start #{inspect(Endpoint)}: port #{port} is already in use"
    end

    test "start/4 logs the port and returns the original reason", %{port: port} do
      log =
        capture_log(fn ->
          assert {:error, :eaddrinuse} = GRPC.Server.start_endpoint(Endpoint, port)
        end)

      assert log =~ "Could not start #{inspect(Endpoint)}: port #{port} is already in use"
    end
  end

  describe "when the address can not be listened on" do
    # No host has 1.2.3.4 as a local address, so listen fails with :eaddrnotavail
    @opts [adapter_opts: [ip: {1, 2, 3, 4}]]

    test "the supervisor logs the endpoint, port and reason", %{port: port} do
      log =
        capture_log(fn ->
          assert {:error, reason} =
                   GRPC.Server.Supervisor.start_link(
                     [endpoint: Endpoint, port: port, start_server: true] ++ @opts
                   )

          assert contains?(reason, :eaddrnotavail)
          assert {:listen_failed, _message, _} = find_listen_failed(reason)
        end)

      assert log =~ "cannot listen on port #{port}"
      assert log =~ "(eaddrnotavail)"
    end

    test "start/4 logs the endpoint, port and reason", %{port: port} do
      log =
        capture_log(fn ->
          assert {:error, reason} = GRPC.Server.start_endpoint(Endpoint, port, @opts)
          assert contains?(reason, :eaddrnotavail)
        end)

      assert log =~ "Could not start #{inspect(Endpoint)}: cannot listen on port #{port}"
      assert log =~ "(eaddrnotavail)"
    end
  end

  test "errors that are not listen failures pass through unchanged" do
    # `:cowboy.start_clear/3` rejects a name that is already taken with a plain tuple
    {:ok, _, _port} = GRPC.Server.start_endpoint(Endpoint, 0)
    on_exit(fn -> GRPC.Server.stop_endpoint(Endpoint) end)

    log =
      capture_log(fn ->
        assert {:error, {:already_started, _pid}} = GRPC.Server.start_endpoint(Endpoint, 0)
      end)

    refute log =~ "Could not start"
  end

  # Supervisor errors nest the reason at a depth that Ranch and OTP choose
  defp contains?(term, needle) when term == needle, do: true
  defp contains?(term, needle) when is_tuple(term), do: contains?(Tuple.to_list(term), needle)
  defp contains?(term, needle) when is_list(term), do: Enum.any?(term, &contains?(&1, needle))
  defp contains?(_, _), do: false

  defp find_listen_failed({:listen_failed, _, _} = found), do: found

  defp find_listen_failed(term) when is_tuple(term),
    do: term |> Tuple.to_list() |> find_listen_failed()

  defp find_listen_failed(term) when is_list(term),
    do: Enum.find_value(term, &find_listen_failed/1)

  defp find_listen_failed(_), do: nil
end

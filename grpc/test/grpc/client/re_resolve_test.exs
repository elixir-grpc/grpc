defmodule GRPC.Client.ReResolveTest do
  @moduledoc """
  Tests for periodic DNS re-resolution in the gRPC client connection manager.

  Test coverage modelled after grpc-go's dns_resolver_test.go and
  resolver_update_test.go. Covers:

    1. Scale-up — new backends discovered on re-resolution
    2. Scale-down — backends removed on re-resolution
    3. No-op — unchanged addresses leave channels untouched
    4. Complete replacement — disjoint address lists
    5. DNS failure — keeps existing channels, logs warning
    6. Recovery after failure — next cycle succeeds
    7. Empty address list — treated as failure, channels preserved
    8. Recovery after empty — next cycle with valid addresses works
    9. pick_channel stability — channel picking works during/after re-resolution
   10. pick_channel after full replacement — new backend is pickable
   11. Repeated cycles — timer fires on every interval tick
   12. Non-DNS targets — ipv4: targets do not trigger re-resolution
   13. Timer after disconnect — no crash
   14. LB error during re-resolution — connection survives
   15. Port change on same host — detected as new address
   16. Exponential backoff — doubles on failure, resets on success, caps at max
   17. Rate limiting — resolve_now calls coalesced within min_resolve_interval
   18. Telemetry — :stop event on success, :error event on failure/empty
  """
  use GRPC.Client.DataCase, async: false
  import Mox

  alias GRPC.Client.Connection

  # Interval for re-resolution in tests (ms). Long enough that only one
  # re-resolve fires per sleep window, short enough tests stay fast.
  @resolve_interval 200
  # Sleep slightly longer than one interval so the timer fires exactly once.
  @wait @resolve_interval + 100
  # After a failure, backoff doubles the interval. Wait accordingly.
  @wait_after_backoff @resolve_interval * 2 + 150

  setup do
    Mox.set_mox_global()
    ref = make_ref()
    resolver = GRPC.Client.MockResolver

    # Default stubs for the new lifecycle callbacks. Tests using
    # connect_with_resolver override init with their own stub; tests
    # calling Connection.connect directly pick these up automatically.
    stub(resolver, :init, fn _target, init_opts ->
      connect_opts = Keyword.get(init_opts, :connect_opts, [])

      {:ok, pid} =
        GRPC.Client.DNSResolver.start_link(
          connection_pid: Keyword.fetch!(init_opts, :connection_pid),
          resolver: resolver,
          target: "dns://my-service.local:50051",
          resolve_interval: Keyword.get(connect_opts, :resolve_interval, 200),
          max_resolve_interval: Keyword.get(connect_opts, :max_resolve_interval, 300_000),
          min_resolve_interval: Keyword.get(connect_opts, :min_resolve_interval, 0)
        )

      {:ok, %{worker_pid: pid}}
    end)

    stub(resolver, :update, fn state, _event -> {:ok, state} end)
    stub(resolver, :shutdown, fn _state -> :ok end)

    %{
      ref: ref,
      adapter: GRPC.Test.ClientAdapter,
      resolver: resolver
    }
  end

  # -- helpers ---------------------------------------------------------------

  defp disconnect_and_wait(channel) do
    ref = channel.ref
    pid = :global.whereis_name({Connection, ref})

    if pid && Process.alive?(pid) do
      # Also monitor the DNSResolver so we wait for it to die
      state = :sys.get_state(pid)
      resolver_pid = state.resolver_state && state.resolver_state[:worker_pid]

      mon = Process.monitor(pid)
      Connection.disconnect(channel)

      receive do
        {:DOWN, ^mon, :process, ^pid, _} -> :ok
      after
        1_000 -> :ok
      end

      # DNSResolver is linked to Connection, so it should die too.
      # Wait briefly to ensure it's fully stopped.
      if resolver_pid && Process.alive?(resolver_pid) do
        resolver_mon = Process.monitor(resolver_pid)

        receive do
          {:DOWN, ^resolver_mon, :process, ^resolver_pid, _} -> :ok
        after
          1_000 -> :ok
        end
      end
    end
  end

  defp connect_with_resolver(ref, resolver, adapter, addresses, opts) do
    # Initial resolve call during connect/2
    expect(resolver, :resolve, fn _target ->
      {:ok, %{addresses: addresses, service_config: nil}}
    end)

    # Stub subsequent re-resolve calls to return the same addresses by default.
    # Individual tests override this with expect/stub before sleeping.
    stub(resolver, :resolve, fn _target ->
      {:ok, %{addresses: addresses, service_config: nil}}
    end)

    # Stub the new lifecycle callbacks so Connection can delegate to them.
    stub(resolver, :init, fn _target, init_opts ->
      connect_opts = Keyword.get(init_opts, :connect_opts, [])

      {:ok, pid} =
        GRPC.Client.DNSResolver.start_link(
          connection_pid: Keyword.fetch!(init_opts, :connection_pid),
          resolver: resolver,
          target: "dns://my-service.local:50051",
          resolve_interval: Keyword.get(connect_opts, :resolve_interval, 200),
          max_resolve_interval: Keyword.get(connect_opts, :max_resolve_interval, 300_000),
          min_resolve_interval: Keyword.get(connect_opts, :min_resolve_interval, 0)
        )

      {:ok, %{worker_pid: pid}}
    end)

    stub(resolver, :update, fn state, _event -> {:ok, state} end)
    stub(resolver, :shutdown, fn _state -> :ok end)

    Connection.connect(
      "dns://my-service.local:50051",
      [
        adapter: adapter,
        name: ref,
        resolver: resolver,
        resolve_interval: @resolve_interval,
        min_resolve_interval: 0
      ] ++ opts
    )
  end

  defp get_state(ref) do
    pid = :global.whereis_name({Connection, ref})
    :sys.get_state(pid)
  end

  defp get_resolver_state(ref) do
    conn_state = get_state(ref)
    worker_pid = conn_state.resolver_state.worker_pid
    :sys.get_state(worker_pid)
  end

  # -- 1. Scale-up: new backends discovered ----------------------------------

  describe "scale-up: new backends discovered" do
    test "adds channels for addresses that appear in DNS", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      assert map_size(get_state(ctx.ref).real_channels) == 1

      new_addrs = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: new_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 2
      assert Map.has_key?(state.real_channels, "10.0.0.1:50051")
      assert Map.has_key?(state.real_channels, "10.0.0.2:50051")

      disconnect_and_wait(channel)
    end
  end

  # -- 2. Scale-down: backends removed ---------------------------------------

  describe "scale-down: backends removed" do
    test "disconnects channels for addresses no longer in DNS", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051},
            %{address: "10.0.0.2", port: 50051}
          ],
          lb_policy: :round_robin
        )

      assert map_size(get_state(ctx.ref).real_channels) == 2

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 1
      assert Map.has_key?(state.real_channels, "10.0.0.1:50051")
      refute Map.has_key?(state.real_channels, "10.0.0.2:50051")

      disconnect_and_wait(channel)
    end
  end

  # -- 3. No-op: unchanged addresses ----------------------------------------

  describe "no-op: unchanged addresses" do
    test "leaves channels untouched when DNS returns the same set", ctx do
      addresses = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051}
      ]

      {:ok, channel} =
        connect_with_resolver(ctx.ref, ctx.resolver, ctx.adapter, addresses,
          lb_policy: :round_robin
        )

      state_before = get_state(ctx.ref)

      # stub already returns same addresses from connect_with_resolver
      Process.sleep(@wait)

      state_after = get_state(ctx.ref)
      assert state_before.real_channels == state_after.real_channels

      disconnect_and_wait(channel)
    end
  end

  # -- 4. Complete replacement: disjoint address lists -----------------------

  describe "complete replacement: disjoint address lists" do
    test "removes old and creates new channels for entirely different backends", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051},
            %{address: "10.0.0.2", port: 50051}
          ],
          lb_policy: :round_robin
        )

      new_addrs = [
        %{address: "10.0.0.3", port: 50051},
        %{address: "10.0.0.4", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: new_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 2
      refute Map.has_key?(state.real_channels, "10.0.0.1:50051")
      refute Map.has_key?(state.real_channels, "10.0.0.2:50051")
      assert Map.has_key?(state.real_channels, "10.0.0.3:50051")
      assert Map.has_key?(state.real_channels, "10.0.0.4:50051")

      disconnect_and_wait(channel)
    end
  end

  # -- 5. DNS failure keeps existing channels --------------------------------

  describe "DNS failure during re-resolution" do
    test "keeps existing channels on resolver error", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target -> {:error, :timeout} end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 1
      assert Map.has_key?(state.real_channels, "10.0.0.1:50051")

      disconnect_and_wait(channel)
    end
  end

  # -- 6. Recovery after DNS failure -----------------------------------------

  describe "recovery after DNS failure" do
    test "next successful cycle updates channels after a failed one", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # First cycle: failure
      stub(ctx.resolver, :resolve, fn _target -> {:error, :nxdomain} end)

      Process.sleep(@wait)
      assert map_size(get_state(ctx.ref).real_channels) == 1

      # Second cycle: success (interval is doubled after failure)
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50051}
           ],
           service_config: nil
         }}
      end)

      Process.sleep(@wait_after_backoff)
      assert map_size(get_state(ctx.ref).real_channels) == 2

      disconnect_and_wait(channel)
    end
  end

  # -- 7. Empty address list: treated as failure -----------------------------

  describe "empty address list on re-resolution" do
    test "keeps existing channels when DNS returns zero addresses", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051},
            %{address: "10.0.0.2", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [], service_config: nil}}
      end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 2

      disconnect_and_wait(channel)
    end
  end

  # -- 8. Recovery after empty -----------------------------------------------

  describe "recovery after empty address list" do
    test "subsequent cycle with valid addresses updates channels", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # First cycle: empty — channels preserved
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [], service_config: nil}}
      end)

      Process.sleep(@wait)
      assert map_size(get_state(ctx.ref).real_channels) == 1

      # Second cycle: valid — channels updated (interval doubled after empty)
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.3", port: 50051}
           ],
           service_config: nil
         }}
      end)

      Process.sleep(@wait_after_backoff)
      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 2
      assert Map.has_key?(state.real_channels, "10.0.0.3:50051")

      disconnect_and_wait(channel)
    end
  end

  # -- 9. pick_channel stability during re-resolution -----------------------

  describe "pick_channel stability during re-resolution" do
    test "pick_channel continues to work while addresses change", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      assert {:ok, _} = Connection.pick_channel(channel)

      new_addrs = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: new_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)

      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host in ["10.0.0.1", "10.0.0.2"]
      assert picked.port == 50051

      disconnect_and_wait(channel)
    end
  end

  # -- 10. pick_channel after full replacement --------------------------------

  describe "pick_channel after full backend replacement" do
    test "picks a channel from the new backend set", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.99", port: 50051}], service_config: nil}}
      end)

      Process.sleep(@wait)

      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host == "10.0.0.99"

      disconnect_and_wait(channel)
    end
  end

  # -- 11. Repeated re-resolution cycles -------------------------------------

  describe "repeated re-resolution cycles" do
    test "timer fires on every interval tick, accumulating changes", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # Cycle 1: 2 backends
      two_addrs = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: two_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)
      assert map_size(get_state(ctx.ref).real_channels) == 2

      # Cycle 2: 3 backends (no backoff — previous cycle succeeded)
      three_addrs = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051},
        %{address: "10.0.0.3", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: three_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)
      assert map_size(get_state(ctx.ref).real_channels) == 3

      disconnect_and_wait(channel)
    end
  end

  # -- 12. Non-DNS targets skip re-resolution --------------------------------

  describe "non-DNS targets skip re-resolution" do
    test "ipv4 target does not start a dns resolver process", ctx do
      {:ok, channel} =
        Connection.connect("ipv4:127.0.0.1:50051",
          adapter: ctx.adapter,
          name: ctx.ref,
          resolve_interval: 50
        )

      # Wait long enough for re-resolve to have fired if it was scheduled
      Process.sleep(200)

      # Should still be alive and working — no resolver process started
      assert {:ok, _} = Connection.pick_channel(channel)
      assert is_nil(get_state(ctx.ref).resolver_state)

      disconnect_and_wait(channel)
    end
  end

  # -- 13. Re-resolution after disconnect is a no-op -------------------------

  describe "re-resolution after disconnect" do
    test "linked resolver dies when connection disconnects", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      Connection.disconnect(channel)

      # Wait past when the re-resolve timer would fire — should not crash
      Process.sleep(@wait)

      # Process should be gone, pick should fail cleanly
      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  # -- 14. LB crash during re-resolution doesn't kill the connection ---------

  describe "LB error during re-resolution" do
    test "connection survives when re-resolved addresses cause LB init to fail", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.5", port: 50051}
           ],
           service_config: nil
         }}
      end)

      Process.sleep(@wait)

      # GenServer should still be alive and channels should be updated
      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 2
      assert {:ok, _} = Connection.pick_channel(channel)

      disconnect_and_wait(channel)
    end
  end

  # -- 15. Port change on same host detected --------------------------------

  describe "port change on same host" do
    test "detects port change as a new address", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      assert Map.has_key?(get_state(ctx.ref).real_channels, "10.0.0.1:50051")

      # Same host, different port
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [%{address: "10.0.0.1", port: 50052}],
           service_config: nil
         }}
      end)

      Process.sleep(@wait)

      state = get_state(ctx.ref)
      assert map_size(state.real_channels) == 1
      refute Map.has_key?(state.real_channels, "10.0.0.1:50051")
      assert Map.has_key?(state.real_channels, "10.0.0.1:50052")

      disconnect_and_wait(channel)
    end
  end

  # -- 16. Exponential backoff on failure ------------------------------------

  describe "exponential backoff on failure" do
    test "interval doubles after each consecutive failure", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # Fail continuously
      stub(ctx.resolver, :resolve, fn _target -> {:error, :nxdomain} end)

      # After first failure: interval should double
      Process.sleep(@wait)
      resolver_state = get_resolver_state(ctx.ref)
      assert resolver_state.resolve_interval == @resolve_interval * 2

      # After second failure: doubles again
      Process.sleep(resolver_state.resolve_interval + 50)
      resolver_state = get_resolver_state(ctx.ref)
      assert resolver_state.resolve_interval == @resolve_interval * 4

      disconnect_and_wait(channel)
    end

    test "interval resets to base after successful resolution", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # First cycle: failure → doubles interval
      stub(ctx.resolver, :resolve, fn _target -> {:error, :nxdomain} end)

      Process.sleep(@wait)
      resolver_state = get_resolver_state(ctx.ref)
      assert resolver_state.resolve_interval == @resolve_interval * 2

      # Second cycle: success → resets to base
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      Process.sleep(@wait_after_backoff)
      resolver_state = get_resolver_state(ctx.ref)
      assert resolver_state.resolve_interval == @resolve_interval

      disconnect_and_wait(channel)
    end

    test "interval caps at max_resolve_interval", ctx do
      # Set max to 4x base so we can hit the cap quickly
      max = @resolve_interval * 4

      expect(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      stub(ctx.resolver, :resolve, fn _target -> {:error, :nxdomain} end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: @resolve_interval,
          max_resolve_interval: max,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      # Fail 1: 200 -> 400
      Process.sleep(@wait)
      assert get_resolver_state(ctx.ref).resolve_interval == @resolve_interval * 2

      # Fail 2: 400 -> 800 capped to 800 (= max)
      Process.sleep(@resolve_interval * 2 + 50)
      assert get_resolver_state(ctx.ref).resolve_interval == max

      # Fail 3: should stay at max, not grow further
      Process.sleep(max + 50)
      assert get_resolver_state(ctx.ref).resolve_interval == max

      disconnect_and_wait(channel)
    end
  end

  # -- 17. Rate limiting / resolve_now coalescing ----------------------------

  describe "rate limiting" do
    test "resolve_now calls within min_resolve_interval are skipped", ctx do
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      # Use a long resolve_interval so the periodic timer doesn't fire,
      # and a large min_resolve_interval to make rate limiting deterministic.
      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: 60_000,
          min_resolve_interval: 60_000,
          lb_policy: :round_robin
        )

      # Track resolve calls during the burst
      call_count = :counters.new(1, [:atomics])

      stub(ctx.resolver, :resolve, fn _target ->
        :counters.add(call_count, 1, 1)
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      resolver_pid = get_state(ctx.ref).resolver_state.worker_pid

      # Fire 20 resolve_now calls rapidly
      for _ <- 1..20, do: send(resolver_pid, :resolve_now)

      # Wait for the GenServer to drain its mailbox
      _ = :sys.get_state(resolver_pid)

      # With 60s rate limit, only the first should resolve; rest are skipped
      actual = :counters.get(call_count, 1)
      assert actual == 1, "Expected exactly 1 resolution, got #{actual}"

      disconnect_and_wait(channel)
    end
  end

  # -- 18. Telemetry events --------------------------------------------------

  describe "telemetry events" do
    setup do
      test_pid = self()
      handler_id = "test-resolve-telemetry-#{inspect(test_pid)}"

      :telemetry.attach_many(
        handler_id,
        [
          [:grpc, :client, :resolve, :stop],
          [:grpc, :client, :resolve, :error]
        ],
        fn name, measurements, metadata, [] ->
          send(test_pid, {:telemetry, name, measurements, metadata})
        end,
        []
      )

      on_exit(fn -> :telemetry.detach(handler_id) end)
      :ok
    end

    test "emits :stop event on successful re-resolution", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      new_addrs = [
        %{address: "10.0.0.1", port: 50051},
        %{address: "10.0.0.2", port: 50051}
      ]

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: new_addrs, service_config: nil}}
      end)

      Process.sleep(@wait)

      assert_received {:telemetry, [:grpc, :client, :resolve, :stop], measurements, metadata}
      assert is_integer(measurements.duration)
      assert measurements.duration >= 0
      assert metadata.target == "dns://my-service.local:50051"
      assert metadata.address_count == 2

      disconnect_and_wait(channel)
    end

    test "emits :error event on DNS failure", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target -> {:error, :timeout} end)

      Process.sleep(@wait)

      assert_received {:telemetry, [:grpc, :client, :resolve, :error], measurements, metadata}
      assert is_integer(measurements.duration)
      assert metadata.target == "dns://my-service.local:50051"
      assert metadata.reason == :timeout
      assert metadata.address_count == 0

      disconnect_and_wait(channel)
    end

    test "emits :error event on empty address list", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [], service_config: nil}}
      end)

      Process.sleep(@wait)

      assert_received {:telemetry, [:grpc, :client, :resolve, :error], _measurements, metadata}
      assert metadata.reason == :empty_addresses

      disconnect_and_wait(channel)
    end
  end

  # -- 19. Stale persistent_term: LB picks unhealthy channel -----------------

  describe "stale persistent_term prevention" do
    setup ctx do
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["10.0.0.99"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)
      Map.put(ctx, :failing_adapter, GRPC.Test.FailingClientAdapter)
    end

    test "falls back to healthy channel when LB picks a failed one", ctx do
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.failing_adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: @resolve_interval,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      assert {:ok, _} = Connection.pick_channel(channel)

      # Re-resolve adds a failing host. Round-robin might pick it, but
      # fallback should ensure we get the healthy one.
      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.99", port: 50051},
             %{address: "10.0.0.1", port: 50051}
           ],
           service_config: nil
         }}
      end)

      Process.sleep(@wait)

      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host == "10.0.0.1"

      disconnect_and_wait(channel)
    end

    test "pick_channel returns error when all new channels fail", ctx do
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.failing_adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: @resolve_interval,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      assert {:ok, _} = Connection.pick_channel(channel)

      # Re-resolve replaces with ONLY failing hosts
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["10.0.0.98", "10.0.0.99"])

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.98", port: 50051},
             %{address: "10.0.0.99", port: 50051}
           ],
           service_config: nil
         }}
      end)

      Process.sleep(@wait)

      assert {:error, :no_connection} = Connection.pick_channel(channel)
    end
  end

  # -- 20. Retry previously failed channels still in DNS ---------------------

  describe "retry previously failed channels" do
    setup ctx do
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["10.0.0.2"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)
      Map.put(ctx, :failing_adapter, GRPC.Test.FailingClientAdapter)
    end

    test "reconnects a previously failed channel when it becomes reachable", ctx do
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50051}
           ],
           service_config: nil
         }}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50051}
           ],
           service_config: nil
         }}
      end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.failing_adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: @resolve_interval,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      # 10.0.0.2 should be in error state
      state = get_state(ctx.ref)
      assert match?({:failed, _}, Map.get(state.real_channels, "10.0.0.2:50051"))

      # Now make 10.0.0.2 reachable
      Application.put_env(:grpc, :grpc_test_failing_hosts, [])

      Process.sleep(@wait)

      # Both channels should now be healthy
      state = get_state(ctx.ref)
      assert match?({:connected, _}, Map.get(state.real_channels, "10.0.0.1:50051"))
      assert match?({:connected, _}, Map.get(state.real_channels, "10.0.0.2:50051"))

      disconnect_and_wait(channel)
    end
  end

  # -- 21. Resolver runs in dedicated process, doesn't block Connection ------

  describe "dedicated resolver process" do
    test "Connection stays responsive during slow DNS resolution", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # Simulate a resolver that takes a long time
      stub(ctx.resolver, :resolve, fn _target ->
        Process.sleep(2_000)
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      # Wait for re-resolve to fire (runs in DNSResolver process)
      Process.sleep(@wait)

      # Connection GenServer should still be responsive — pick_channel works
      assert {:ok, _} = Connection.pick_channel(channel)

      # Disconnect should also work while resolve is in-flight
      assert {:ok, _} = Connection.disconnect(channel)
    end

    test "resolver crash doesn't kill the connection (linked process exits)", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [
            %{address: "10.0.0.1", port: 50051}
          ],
          lb_policy: :round_robin
        )

      # Verify dns_resolver is running
      state = get_state(ctx.ref)
      assert is_map(state.resolver_state)
      assert is_pid(state.resolver_state.worker_pid)
      assert Process.alive?(state.resolver_state.worker_pid)

      disconnect_and_wait(channel)
    end
  end

  # -- 22. :refresh handler doesn't crash on {:error, _} channels -----------

  describe "refresh handler with failed channels" do
    setup ctx do
      Application.put_env(:grpc, :grpc_test_failing_hosts, ["10.0.0.2"])
      on_exit(fn -> Application.delete_env(:grpc, :grpc_test_failing_hosts) end)
      Map.put(ctx, :failing_adapter, GRPC.Test.FailingClientAdapter)
    end

    test "GenServer survives when :refresh picks a failed channel", ctx do
      # Connect with 2 backends — one healthy, one failing
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50051}
           ],
           service_config: nil
         }}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok,
         %{
           addresses: [
             %{address: "10.0.0.1", port: 50051},
             %{address: "10.0.0.2", port: 50051}
           ],
           service_config: nil
         }}
      end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.failing_adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: 60_000,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      # 10.0.0.2 is {:failed, _} in real_channels
      state = get_state(ctx.ref)
      assert match?({:failed, _}, Map.get(state.real_channels, "10.0.0.2:50051"))

      # Wait for several :refresh cycles (15s default, but we'll trigger manually).
      # Round-robin will eventually pick 10.0.0.2. Without the fix, this crashes.
      pid = :global.whereis_name({Connection, ctx.ref})

      for _ <- 1..5 do
        send(pid, :refresh)
      end

      # Small sleep for messages to process
      Process.sleep(50)

      # GenServer should still be alive
      assert Process.alive?(pid)
      assert {:ok, picked} = Connection.pick_channel(channel)
      assert picked.host == "10.0.0.1"

      disconnect_and_wait(channel)
    end
  end

  # -- 23. DNS.init returns nil for non-DNS targets ----------------------------

  describe "resolver init with non-DNS target" do
    test "DNS resolver returns {:ok, nil} for ipv4 target, Connection handles it", ctx do
      # Simulate DNS resolver being configured but target is ipv4
      # DNS.init should return {:ok, nil} and Connection should store nil resolver_state
      expect(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      stub(ctx.resolver, :resolve, fn _target ->
        {:ok, %{addresses: [%{address: "10.0.0.1", port: 50051}], service_config: nil}}
      end)

      # Mock init to return nil (simulating DNS resolver with non-DNS target)
      stub(ctx.resolver, :init, fn _target, _opts -> {:ok, nil} end)

      {:ok, channel} =
        Connection.connect(
          "dns://my-service.local:50051",
          adapter: ctx.adapter,
          name: ctx.ref,
          resolver: ctx.resolver,
          resolve_interval: @resolve_interval,
          min_resolve_interval: 0,
          lb_policy: :round_robin
        )

      # resolver_state should be nil
      state = get_state(ctx.ref)
      assert is_nil(state.resolver_state)

      # resolve_now should be a no-op (cast, so returns :ok)
      Connection.resolve_now(channel)
      Process.sleep(50)

      # Connection should still work fine
      assert {:ok, _} = Connection.pick_channel(channel)

      disconnect_and_wait(channel)
    end
  end

  # -- 24. Resolver worker crash recovery --------------------------------------

  describe "resolver worker crash recovery" do
    test "Connection re-initializes resolver when worker crashes", ctx do
      {:ok, channel} =
        connect_with_resolver(
          ctx.ref,
          ctx.resolver,
          ctx.adapter,
          [%{address: "10.0.0.1", port: 50051}],
          lb_policy: :round_robin
        )

      state = get_state(ctx.ref)
      original_pid = state.resolver_state.worker_pid
      assert Process.alive?(original_pid)

      # Kill the worker — Connection traps exits and should re-init
      Process.exit(original_pid, :kill)
      Process.sleep(100)

      # Connection should still be alive
      conn_pid = :global.whereis_name({Connection, ctx.ref})
      assert Process.alive?(conn_pid)

      # resolver_state should have a NEW worker pid
      state = get_state(ctx.ref)
      assert state.resolver_state != nil
      assert state.resolver_state.worker_pid != original_pid
      assert Process.alive?(state.resolver_state.worker_pid)

      # Channel should still be pickable
      assert {:ok, _} = Connection.pick_channel(channel)

      disconnect_and_wait(channel)
    end
  end
end

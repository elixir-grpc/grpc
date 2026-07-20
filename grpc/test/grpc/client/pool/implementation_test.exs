defmodule GRPC.Client.Pool.ImplementationTest do
  use ExUnit.Case, async: false

  defmodule HelloServer do
    use GRPC.Server, service: Helloworld.Greeter.Service

    def say_hello(req, _stream) do
      %Helloworld.HelloReply{message: "Hello, #{req.name}"}
    end
  end

  alias GRPC.Client.Pool.Config
  alias GRPC.Client.Pool.Server.State

  @subject GRPC.Client.Pool.Implementation

  setup_all do
    {:ok, _, port} = GRPC.Server.start(HelloServer, 0, max_concurrent_streams: 1_000)
    on_exit(fn -> :ok = GRPC.Server.stop(HelloServer) end)
    %{port: port}
  end

  describe "init/1" do
    setup %{port: port} do
      config = make_config(port)
      %{state: %State{config: config}}
    end

    test "opens N amount of channels, where N is a value of pool_size specified in config", %{
      state: state
    } do
      %State{channels: wrapped_channels} = @subject.init(state)

      assert wrapped_channels
             |> Enum.map(fn {_id, wrapped} -> wrapped.channel end)
             |> Enum.count() == state.config.pool_size

      assert Enum.all?(wrapped_channels, fn {_id, %State.Channel{channel: channel}} ->
               is_struct(channel, GRPC.Channel)
             end)
    end

    test "all channels should have 0 streams", %{state: state} do
      %State{channels: wrapped_channels} = @subject.init(state)

      assert Enum.all?(wrapped_channels, fn {_id, %State.Channel{open_streams: open_streams}} ->
               open_streams == 0
             end)
    end
  end

  describe "take_channel/2" do
    setup %{port: port} do
      config = make_config(port)
      %{state: @subject.init(%State{config: config})}
    end

    test "returns a first channel that has < max_number_of_streams_per_connection", %{
      state: state
    } do
      {channels, _state, _pids, _refs} = take_channels_n_times(100, state)
      assert length(channels) == 100

      assert [_its_the_same_channel_for] =
               Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)
    end

    test "when channel has opened max_number_of_streams_per_connection next channel from pool is returned",
         %{state: state} do
      {channels, state, _pids, _refs} = take_channels_n_times(100, state)

      [%State.Channel{id: max_streams_channel_id}] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {%State.Channel{id: next_channel_in_the_pool_id}, _monitor_ref, _state} =
               @subject.take_channel(state, :c.pid(0, 1, 0))

      refute max_streams_channel_id == next_channel_in_the_pool_id
    end

    test "opening channel adds to leases by channel id", %{state: state} do
      {channels, state, [expected_pid1, expected_pid2, expected_pid3],
       [expected_ref1, expected_ref2, expected_ref3]} =
        take_channels_n_times(3, state)

      [%State.Channel{id: channel_id}] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert %State{
               leases_by_channel_id: %{
                 ^channel_id => [
                   %State.Lease{monitor_ref: ^expected_ref1, caller_pid: ^expected_pid1},
                   %State.Lease{monitor_ref: ^expected_ref2, caller_pid: ^expected_pid2},
                   %State.Lease{monitor_ref: ^expected_ref3, caller_pid: ^expected_pid3}
                 ]
               }
             } = state
    end

    test "opening channel adds to leases by monitor_ref", %{state: state} do
      {channels, state, [expected_pid1, expected_pid2, expected_pid3],
       [expected_ref1, expected_ref2, expected_ref3]} =
        take_channels_n_times(3, state)

      [%State.Channel{id: channel_id}] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert %State{
               leases_by_monitor_ref: %{
                 ^expected_ref1 => %State.Lease{id: ^channel_id, caller_pid: ^expected_pid1},
                 ^expected_ref2 => %State.Lease{id: ^channel_id, caller_pid: ^expected_pid2},
                 ^expected_ref3 => %State.Lease{id: ^channel_id, caller_pid: ^expected_pid3}
               }
             } = state
    end

    test "when there is no free channel in the pool, new one is opened, and it's id is added to leases_by_channel_id with empty array",
         %{state: state} do
      [old_key] = Map.keys(state[:leases_by_channel_id])
      {_channels, state, _pids, _refs} = take_channels_n_times(101, state)

      [new_key] =
        state[:leases_by_channel_id] |> Map.keys() |> Enum.reject(fn k -> k == old_key end)

      assert [%State.Lease{}] = state[:leases_by_channel_id][new_key]
    end
  end

  describe "take_channel/3" do
    setup %{port: port} do
      config = make_config(port)
      %{state: @subject.init(%State{config: config})}
    end

    test "when channel is requested by it's ID it is returned if it hasn't reached max_number_of_streams_per_connection",
         %{state: state} do
      {channels, state, _pids, _refs} = take_channels_n_times(99, state)

      assert [%{id: channel_id} = _channel] =
               Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {%{id: ^channel_id}, _monitor_ref, _state} =
               @subject.take_channel(state, channel_id, :c.pid(0, 0, 100))
    end

    test "when channel is requested by it's ID if it has reached max_number_of_streams_per_connection nil is returned",
         %{state: state} do
      {channels, state, _pids, _refs} = take_channels_n_times(100, state)

      assert [%{id: channel_id} = _channel] =
               Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {nil = _channel, nil = _monitor_ref, ^state} =
               @subject.take_channel(state, channel_id, :c.pid(0, 0, 100))
    end
  end

  describe "return_channel/3" do
    setup %{port: port} do
      config = make_config(port)
      %{state: %State{config: config}}
    end

    test "returning a channel decrements it's stream count and makes it available to other clients",
         %{state: state} do
      {channels, state, [pid | _], [ref | _]} = take_channels_n_times(100, state)

      [%State.Channel{id: max_streams_channel_id} = channel] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {state, ^ref} = @subject.return_channel(state, channel, pid)

      assert {%State.Channel{id: next_channel_in_the_pool_id}, _monitor_ref, _state} =
               @subject.take_channel(state, pid)

      assert max_streams_channel_id == next_channel_in_the_pool_id
    end

    test "returning a channel removes from leases by channel id", %{state: state} do
      {channels, state, [expected_pid1, expected_pid2, expected_pid3],
       [expected_ref1, expected_ref2, expected_ref3]} =
        take_channels_n_times(3, state)

      [%State.Channel{id: channel_id} = channel] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {state, ^expected_ref1} = @subject.return_channel(state, channel, expected_pid1)

      assert %State{
               leases_by_channel_id: %{
                 ^channel_id => [
                   %State.Lease{monitor_ref: ^expected_ref2, caller_pid: ^expected_pid2},
                   %State.Lease{monitor_ref: ^expected_ref3, caller_pid: ^expected_pid3}
                 ]
               }
             } = state
    end

    test "returning a channel removes from leases by monitor ref", %{state: state} do
      {channels, state, [expected_pid1, expected_pid2, expected_pid3],
       [expected_ref1, expected_ref2, expected_ref3]} =
        take_channels_n_times(3, state)

      [%State.Channel{id: channel_id} = channel] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {state, ^expected_ref1} = @subject.return_channel(state, channel, expected_pid1)

      assert %State{
               leases_by_monitor_ref: %{
                 ^expected_ref2 => %State.Lease{id: ^channel_id, caller_pid: ^expected_pid2},
                 ^expected_ref3 => %State.Lease{id: ^channel_id, caller_pid: ^expected_pid3}
               }
             } = state
    end
  end

  describe "handle_connection_process_crush/2" do
    setup %{port: port} do
      config = make_config(port)
      %{state: %State{config: config}}
    end

    test "when connection process crushes, related channel is removed from the list, all leases are cleared for this channel",
         %{state: state} do
      {channels, state, _pids, [expected_ref1, expected_ref2, expected_ref3] = refs} =
        take_channels_n_times(3, state)

      [
        %State.Channel{
          id: crushed_channel_id,
          channel: %GRPC.Channel{adapter_payload: %{conn_pid: crushed_pid}}
        }
      ] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert %State{
               channels: %{^crushed_channel_id => %State.Channel{}},
               leases_by_channel_id: %{^crushed_channel_id => [_, _, _]},
               leases_by_monitor_ref: %{
                 ^expected_ref1 => _,
                 ^expected_ref2 => _,
                 ^expected_ref3 => _
               }
             } = state

      assert {state, [^expected_ref1, ^expected_ref2, ^expected_ref3]} =
               @subject.handle_connection_process_crush(state, crushed_pid)

      assert %State{
               channels: channels,
               leases_by_channel_id: leases_by_ch_id,
               leases_by_monitor_ref: leases_by_monitor_ref
             } = state

      refute Map.has_key?(channels, crushed_channel_id)
      refute Map.has_key?(leases_by_ch_id, crushed_channel_id)
      assert Enum.all?(refs, &(not Map.has_key?(leases_by_monitor_ref, &1)))
    end

    test "only crushed channel leases are removed", %{state: state} do
      {channels, state, _pids, [survive_ref | expected_crush_refs]} =
        take_channels_n_times(101, state)

      [
        %State.Channel{id: survive_channel_id},
        %State.Channel{channel: %GRPC.Channel{adapter_payload: %{conn_pid: crushed_pid}}}
      ] =
        Enum.uniq_by(channels, fn %State.Channel{id: id} -> id end)

      assert {state, crush_refs} = @subject.handle_connection_process_crush(state, crushed_pid)
      assert Enum.sort(crush_refs) == Enum.sort(expected_crush_refs)

      assert %State{
               channels: channels,
               leases_by_channel_id: leases_by_ch_id,
               leases_by_monitor_ref: leases_by_monitor_ref
             } = state

      assert Map.has_key?(channels, survive_channel_id)
      assert Map.has_key?(leases_by_ch_id, survive_channel_id)
      assert Map.has_key?(leases_by_monitor_ref, survive_ref)
    end

    test "if channel not found in state, state is unchanged", %{state: state} do
      {_channels, state, _pids, refs} = take_channels_n_times(101, state)

      assert {state, []} = @subject.handle_connection_process_crush(state, :c.pid(0, 0, 0))

      assert %State{leases_by_monitor_ref: leases} = state
      assert Enum.all?(refs, &Map.has_key?(leases, &1))
    end
  end

  defp make_config(port) do
    %Config{
      pool_ref: make_ref(),
      channel: %GRPC.Channel{
        host: "localhost",
        port: port,
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

  defp take_channels_n_times(n, state) do
    Enum.reduce(1..n, {[], state, [], []}, fn itt, {channels_acc, state_acc, pid_acc, ref_acc} ->
      pid = :c.pid(0, 0, itt)
      {channel, ref, state} = @subject.take_channel(state_acc, pid)
      {[channel | channels_acc], state, [pid | pid_acc], [ref | ref_acc]}
    end)
  end
end

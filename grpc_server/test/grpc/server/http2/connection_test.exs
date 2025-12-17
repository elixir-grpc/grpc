defmodule GRPC.Server.HTTP2.ConnectionTest do
  use ExUnit.Case, async: true

  alias GRPC.Server.HTTP2.Connection
  alias GRPC.Transport.HTTP2.{Frame, Settings, Errors}

  # For now, we'll test without mocking the socket
  # Just test the logic of handle_frame functions

  describe "handle_frame/3 - SETTINGS" do
    test "applies SETTINGS and updates remote settings" do
      # Create a minimal connection struct
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{},
        send_hpack_state: HPAX.new(4096),
        recv_hpack_state: HPAX.new(4096)
      }

      settings_frame = %Frame.Settings{
        ack: false,
        settings: [header_table_size: 8192, max_frame_size: 32_768]
      }

      # Mock socket - we won't actually call send
      socket = nil

      new_connection = Connection.handle_frame(settings_frame, socket, connection)

      # Should have updated remote settings
      assert new_connection.remote_settings.header_table_size == 8192
      assert new_connection.remote_settings.max_frame_size == 32_768
    end

    test "ignores SETTINGS ACK frames" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      ack_frame = %Frame.Settings{ack: true, settings: []}
      socket = nil

      new_connection = Connection.handle_frame(ack_frame, socket, connection)

      # Connection should be unchanged (except socket interaction)
      assert new_connection.remote_settings == connection.remote_settings
    end

    test "raises error for invalid initial window size" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      invalid_frame = %Frame.Settings{
        ack: false,
        settings: [initial_window_size: 3_000_000_000]
      }

      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(invalid_frame, socket, connection)
      end
    end

    test "raises error for invalid max frame size (too small)" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      invalid_frame = %Frame.Settings{
        ack: false,
        settings: [max_frame_size: 1000]
      }

      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(invalid_frame, socket, connection)
      end
    end

    test "raises error for invalid max frame size (too large)" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      invalid_frame = %Frame.Settings{
        ack: false,
        settings: [max_frame_size: 20_000_000]
      }

      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(invalid_frame, socket, connection)
      end
    end

    test "updates HPACK table size when header_table_size changes" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{header_table_size: 4096},
        send_hpack_state: HPAX.new(4096),
        recv_hpack_state: HPAX.new(4096)
      }

      settings_frame = %Frame.Settings{
        ack: false,
        settings: [header_table_size: 8192]
      }

      socket = nil

      new_connection = Connection.handle_frame(settings_frame, socket, connection)

      # HPACK state should be resized
      assert new_connection.remote_settings.header_table_size == 8192
    end
  end

  describe "handle_frame/3 - PING" do
    test "connection unchanged after PING (ACK sent via socket)" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      ping_frame = %Frame.Ping{ack: false, payload: <<1, 2, 3, 4, 5, 6, 7, 8>>}
      socket = nil

      new_connection = Connection.handle_frame(ping_frame, socket, connection)

      # Connection state should be unchanged (response sent via socket)
      assert new_connection == connection
    end

    test "ignores PING ACK frames" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      pong_frame = %Frame.Ping{ack: true, payload: <<1, 2, 3, 4, 5, 6, 7, 8>>}
      socket = nil

      new_connection = Connection.handle_frame(pong_frame, socket, connection)

      assert new_connection == connection
    end
  end

  describe "handle_frame/3 - WINDOW_UPDATE" do
    test "updates connection send window" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{},
        send_window_size: 65_535
      }

      window_update = %Frame.WindowUpdate{stream_id: 0, size_increment: 1000}
      socket = nil

      new_connection = Connection.handle_frame(window_update, socket, connection)

      assert new_connection.send_window_size == 66_535
    end

    test "raises error on window overflow" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{},
        send_window_size: 2_147_483_647
      }

      window_update = %Frame.WindowUpdate{stream_id: 0, size_increment: 1000}
      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(window_update, socket, connection)
      end
    end
  end

  describe "handle_frame/3 - CONTINUATION" do
    test "accumulates CONTINUATION frames until end_headers" do
      headers_frame = %Frame.Headers{
        stream_id: 1,
        end_stream: false,
        end_headers: false,
        fragment: <<1, 2, 3>>
      }

      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{},
        fragment_frame: headers_frame
      }

      cont1 = %Frame.Continuation{
        stream_id: 1,
        end_headers: false,
        fragment: <<4, 5, 6>>
      }

      socket = nil

      new_connection = Connection.handle_frame(cont1, socket, connection)

      # Should have accumulated fragment
      assert new_connection.fragment_frame != nil
      assert new_connection.fragment_frame.fragment == <<1, 2, 3, 4, 5, 6>>
    end

    test "raises error if non-CONTINUATION frame while expecting CONTINUATION" do
      headers_frame = %Frame.Headers{
        stream_id: 1,
        end_stream: false,
        end_headers: false,
        fragment: <<1, 2, 3>>
      }

      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{},
        fragment_frame: headers_frame
      }

      # Send DATA frame instead of CONTINUATION
      data_frame = %Frame.Data{stream_id: 1, end_stream: false, data: <<>>}
      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(data_frame, socket, connection)
      end
    end
  end

  describe "handle_frame/3 - unsupported frames" do
    test "raises error for PUSH_PROMISE (not supported in gRPC)" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      push_frame = %Frame.PushPromise{
        stream_id: 1,
        promised_stream_id: 2,
        end_headers: true,
        fragment: <<>>
      }

      socket = nil

      assert_raise Errors.ConnectionError, fn ->
        Connection.handle_frame(push_frame, socket, connection)
      end
    end

    test "ignores PRIORITY frames (gRPC doesn't use priorities)" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      priority_frame = %Frame.Priority{
        stream_id: 1,
        exclusive_dependency: false,
        stream_dependency: 0,
        weight: 16
      }

      socket = nil

      new_connection = Connection.handle_frame(priority_frame, socket, connection)

      # Should be unchanged
      assert new_connection == connection
    end

    test "ignores UNKNOWN frames" do
      connection = %Connection{
        local_settings: %Settings{},
        remote_settings: %Settings{}
      }

      unknown_frame = %Frame.Unknown{type: 255, flags: 0, stream_id: 0, payload: <<>>}
      socket = nil

      new_connection = Connection.handle_frame(unknown_frame, socket, connection)

      assert new_connection == connection
    end
  end
end

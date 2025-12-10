defmodule GRPC.Transport.HTTP2.SettingsTest do
  use ExUnit.Case, async: true

  alias GRPC.Transport.HTTP2.Settings

  describe "default settings" do
    test "has correct default values" do
      settings = %Settings{}

      assert settings.header_table_size == 4_096
      assert settings.max_concurrent_streams == :infinity
      assert settings.initial_window_size == 65_535
      assert settings.max_frame_size == 16_384
      assert settings.max_header_list_size == :infinity
    end
  end

  describe "settings modification" do
    test "can update header_table_size" do
      settings = %Settings{header_table_size: 8_192}
      assert settings.header_table_size == 8_192
    end

    test "can update max_concurrent_streams" do
      settings = %Settings{max_concurrent_streams: 100}
      assert settings.max_concurrent_streams == 100
    end

    test "can update initial_window_size" do
      settings = %Settings{initial_window_size: 32_768}
      assert settings.initial_window_size == 32_768
    end

    test "can update max_frame_size" do
      settings = %Settings{max_frame_size: 32_768}
      assert settings.max_frame_size == 32_768
    end

    test "can update max_header_list_size" do
      settings = %Settings{max_header_list_size: 16_384}
      assert settings.max_header_list_size == 16_384
    end
  end
end

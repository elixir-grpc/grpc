defmodule GRPC.Client.Interceptors.LoggerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias GRPC.Client.Interceptors.Logger, as: LoggerInterceptor
  alias GRPC.Client.Stream

  test "accepted_comparators filter logs correctly" do
    for {configured_level, accepted_comparators, should_log} <-
          [
            {:error, [:lt], true},
            {:error, [:eq], false},
            {:error, [:gt], false},
            {:debug, [:eq], false},
            {:debug, [:eq, :gt], false},
            {:info, [:lt, :eq], true}
          ] do
      logger_level = Logger.level()
      assert logger_level == :info

      service_name = "service_name"
      rpc = {1, 2, 3}

      logs =
        capture_log(fn ->
          stream = %Stream{grpc_type: :unary, rpc: rpc, service_name: service_name}

          LoggerInterceptor.call(
            stream,
            :request,
            fn ^stream, :request -> {:ok, :ok} end,
            LoggerInterceptor.init(
              level: configured_level,
              accepted_comparators: accepted_comparators
            )
          )
        end)

      if should_log do
        assert Regex.match?(
                 ~r/\[#{configured_level}\]\s+Call #{to_string(elem(rpc, 0))} of #{service_name}/,
                 logs
               )
      else
        assert logs == ""
      end
    end
  end
end

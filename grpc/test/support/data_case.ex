defmodule GRPC.Client.DataCase do
  @moduledoc """
  This module defines the test case to be used by
  tests for grpc.
  """
  use ExUnit.CaseTemplate

  using do
    quote do
      import GRPC.Factory
      import GRPC.Client.DataCase
    end
  end

  @doc """
  Attaches a telemetry handler for `event` that forwards emissions to the
  test process as `{:telemetry, event, measurements, metadata}` messages,
  for use with `assert_receive`. The handler is detached on test exit.
  """
  def attach_telemetry(event) do
    handler_id = {__MODULE__, self(), System.unique_integer()}
    test_pid = self()

    :telemetry.attach(
      handler_id,
      event,
      fn event, measurements, metadata, _config ->
        send(test_pid, {:telemetry, event, measurements, metadata})
      end,
      nil
    )

    ExUnit.Callbacks.on_exit(fn -> :telemetry.detach(handler_id) end)
  end
end

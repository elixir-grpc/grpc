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

  @doc """
  Polls `fun` until it returns a truthy value or the retries run out,
  returning the last result. Only for effects with no observable signal to
  await, e.g. `Registry` unregistration or another process's internal state
  via `:sys.get_state/1`; prefer `assert_receive` on a message or telemetry
  event when one exists.
  """
  def eventually(fun, retries \\ 50)

  def eventually(fun, 0), do: fun.()

  def eventually(fun, retries) do
    if fun.() do
      true
    else
      Process.sleep(50)
      eventually(fun, retries - 1)
    end
  end
end

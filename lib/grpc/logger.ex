defmodule GRPC.Logger do
  @doc """
  Normalizes the exception and stacktrace inputs by its kind to match the format specified for `crash_report` metadata
  in [Logger](https://hexdocs.pm/logger/main/Logger.html#module-metadata)
  """
  def crash_reason(:throw, reason, stacktrace), do: {{:nocatch, reason}, stacktrace}
  def crash_reason(:error, reason, stack), do: {Exception.normalize(:error, reason, stack), stack}
  def crash_reason(:exit, reason, stacktrace), do: {reason, stacktrace}
end

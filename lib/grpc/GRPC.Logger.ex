defmodule GRPC.Logger do
  def crash_reason(:throw, reason, stacktrace), do: {{:nocatch, reason}, stacktrace}

  def crash_reason(:error, reason, stacktrace),
    do: {Exception.normalize(:error, reason, stacktrace), stacktrace}

  def crash_reason(:exit, reason, stacktrace), do: {reason, stacktrace}
end

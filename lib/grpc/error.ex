defmodule GRPC.TimeoutError do
  defexception [message: "4: deadline exceeded", code: 4]
end

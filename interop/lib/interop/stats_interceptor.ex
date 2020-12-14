defmodule Interop.ServerInterceptor.Statix do
  use Statix
end

defmodule Interop.ServerInterceptor do
  use GRPCStatsd.ServerInterceptor, statsd_module: Interop.ServerInterceptor.Statix
end

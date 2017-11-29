defmodule FooReplacer do
  def call(service_mod, stream, {_, {req_mod, req_stream}, {res_mod, res_stream}} = rpc, func_name, next) do
    IO.puts("FooReplacer.call/4")

    unmarshal_func = fn(req) ->
      case service_mod.unmarshal(req_mod, req) do
        %Helloworld.HelloRequest{name: "grpc-elixir"} -> %Helloworld.HelloRequest{name: "GRPC-elixir"}
        reply -> reply
      end
    end

    next.(service_mod, %{stream | unmarshal: unmarshal_func}, rpc, func_name)
  end
end

defmodule Logging do
  require Logger
  def call(service_mod, stream, rpc, func_name, next) do
    case next.(service_mod, stream, rpc, func_name) do
      {:ok, _stream, _res}    = r -> Logger.info("#{service_mod}.#{func_name} handled with success"); r
      {:error, _stream, _res} = r -> Logger.error("#{service_mod}.#{func_name} handled with failure"); r
      r                           -> Logger.error("#{r}"); r
    end
  end
end

defmodule Metrics do
  require Logger
  def call(service_mod, stream, rpc, func_name, next) do
    {t, ret} = :timer.tc fn ->
      next.(service_mod, stream, rpc, func_name)
    end

    Logger.info("replied in #{t}μs")
    ret
  end
end

defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service, middlewares: [FooReplacer, Logging, Metrics]

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end

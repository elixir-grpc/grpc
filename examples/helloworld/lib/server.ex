defmodule Foo1 do
  def call(service_mod, stream, rpc, func_name, next) do
    IO.puts("Foo1")
    next.(service_mod, stream, rpc, func_name)
  end
end

defmodule Foo2 do
  def call(service_mod, stream, rpc, func_name, next) do
    IO.puts("Foo2")
    next.(service_mod, stream, rpc, func_name)
  end
end

defmodule Foo3 do
  def call(service_mod, stream, rpc, func_name, next) do
    IO.puts("Foo3")
    next.(service_mod, stream, rpc, func_name)
  end
end

defmodule Bar do
  def run do
    wrapped_call = fn(a,b,c,d) -> IO.puts("final") end
    f = Middleware.build_chain([Foo1, Foo2, Foo3], wrapped_call)
    f.(1,2,3,4)
  end
end

defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service, middlewares: [Foo1, Foo2, Foo3]

  @spec say_hello(Helloworld.HelloRequest.t, GRPC.Server.Stream.t) :: Helloworld.HelloReply.t
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(message: "Hello #{request.name}")
  end
end

defmodule GRPC.Service do
  @moduledoc """
  Define gRPC service used by Stub and Server. You should use `Protobuf` to
  to generate code instead of using this module directly.

  It imports DSL functions like `rpc/4` and `stream/1` for defining the RPC
  functions easily:

      defmodule Greeter.Service do
        use GRPC.Service, name: "helloworld.Greeter"

        rpc :SayHello, HelloRequest, stream(HelloReply)
      end

  `option (google.api.http)` annotations are supported for gRPC http/json transcoding. Once generated the 4th argument to `rpc/4` contains
  the `Google.Api.HttpRule` option.

      defmodule Greeter.Service do
        use GRPC.Service, name: "helloworld.Greeter"

        rpc(:SayHello, Helloworld.HelloRequest, Helloworld.HelloReply, %{
          http: %{
            type: Google.Api.PbExtension,
            value: %Google.Api.HttpRule{
              __unknown_fields__: [],
              additional_bindings: [],
              body: "",
              pattern: {:get, "/v1/greeter/{name}"},
              response_body: "",
              selector: ""
            }
          }
        })
      end
  """

  defmacro __using__(opts) do
    opts = Keyword.validate!(opts, [:name, :protoc_gen_elixir_version])

    quote do
      import GRPC.Service, only: [rpc: 4, rpc: 3, stream: 1]

      Module.register_attribute(__MODULE__, :rpc_calls, accumulate: true)
      @before_compile GRPC.Service

      def __meta__(:name), do: unquote(opts[:name])
    end
  end

  defmacro __before_compile__(env) do
    rpc_calls = Module.get_attribute(env.module, :rpc_calls)

    quote do
      def __rpc_calls__, do: unquote(rpc_calls |> Macro.escape() |> Enum.reverse())
    end
  end

  defmacro rpc(name, request, reply, options \\ quote(do: %{})) do
    quote do
      @rpc_calls {unquote(name), unquote(wrap_stream(request)), unquote(wrap_stream(reply)),
                  unquote(options)}
    end
  end

  @doc """
  Specify if the request/reply is streaming.
  """
  def stream(param) do
    quote do: {unquote(param), true}
  end

  @doc false
  def wrap_stream({:stream, _, _} = param) do
    quote do: unquote(param)
  end

  def wrap_stream(param) do
    quote do: {unquote(param), false}
  end

  def grpc_type({_, {_, false}, {_, false}, _}), do: :unary
  def grpc_type({_, {_, true}, {_, false}, _}), do: :client_stream
  def grpc_type({_, {_, false}, {_, true}, _}), do: :server_stream
  def grpc_type({_, {_, true}, {_, true}, _}), do: :bidirectional_stream

  def rpc_options({_, _, _, options}), do: options
  def rpc_options({_, _, _, options}, type), do: Map.get(options, type)
end

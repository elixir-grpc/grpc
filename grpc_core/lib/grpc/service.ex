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

    # Detect collisions in underscored function names
    {annotated_rpc_calls, collisions} = detect_name_collisions(rpc_calls)

    quote do
      def __rpc_calls__, do: unquote(annotated_rpc_calls |> Macro.escape() |> Enum.reverse())
      def __rpc_name_collisions__, do: unquote(collisions |> Macro.escape())
    end
  end

  @doc false
  def detect_name_collisions(rpc_calls) do
    # Group RPCs by their underscored names
    grouped =
      Enum.group_by(rpc_calls, fn {name, _, _, _} ->
        name |> to_string() |> Macro.underscore()
      end)

    # Find which underscored names have collisions, keeping original names
    collision_map =
      grouped
      |> Enum.filter(fn {_, rpcs} -> length(rpcs) > 1 end)
      |> Enum.map(fn {underscored_name, rpcs} ->
        original_names = Enum.map(rpcs, fn {name, _, _, _} -> name end)
        {underscored_name, original_names}
      end)
      |> Map.new()

    # Annotate each RPC call with collision info
    annotated_rpc_calls =
      rpc_calls
      |> Enum.map(fn {name, req, res, options} = rpc ->
        underscored_name = name |> to_string() |> Macro.underscore()

        if Map.has_key?(collision_map, underscored_name) do
          # Mark this RPC as having a collision
          updated_options = Map.put(options, :__name_collision__, true)
          {name, req, res, updated_options}
        else
          rpc
        end
      end)

    {annotated_rpc_calls, collision_map}
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

  @doc """
  Format the function name for display/logging purposes.

  If there's a name collision, returns "call(:Name, ...)" format to reflect actual usage.
  Otherwise, returns the underscored name.

  Handles both full 4-element RPC tuples and partial tuples (for testing).
  """
  def format_function_name(rpc) when is_tuple(rpc) do
    name = elem(rpc, 0)

    # Check if we have a full RPC tuple with options
    has_collision? =
      if tuple_size(rpc) >= 4 do
        options = elem(rpc, 3)
        Map.get(options, :__name_collision__, false)
      else
        false
      end

    if has_collision? do
      "call(#{inspect(name)}, ...)"
    else
      name |> to_string() |> Macro.underscore()
    end
  end
end

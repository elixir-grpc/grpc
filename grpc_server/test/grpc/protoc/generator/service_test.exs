defmodule GRPC.Protoc.Generator.ServiceTest do
  use ExUnit.Case, async: true

  alias GRPC.Protoc.CLI
  alias GRPC.Protoc.Generator.Service
  alias Protobuf.Protoc.Context

  describe "generate/2" do
    test "generates service and stub modules by default" do
      {_module, content} = Service.generate(context(), service())

      assert content =~ "defmodule Helloworld.Greeter.Service do"
      assert content =~ "defmodule Helloworld.Greeter.Stub do"
      assert content =~ "use GRPC.Stub, service: Helloworld.Greeter.Service"
    end

    test "generates service module without stub when gen_stubs=false" do
      ctx = CLI.parse_params(context(), "gen_stubs=false")

      {_module, content} = Service.generate(ctx, service())

      assert content =~ "defmodule Helloworld.Greeter.Service do"
      refute content =~ "defmodule Helloworld.Greeter.Stub do"
      refute content =~ "use GRPC.Stub"
    end
  end

  describe "parse_params/2" do
    test "accepts gen_stubs=true and gen_stubs=false" do
      assert CLI.gen_stubs?(CLI.parse_params(context(), "gen_stubs=true"))
      refute CLI.gen_stubs?(CLI.parse_params(context(), "gen_stubs=false"))
      refute CLI.gen_stubs?(CLI.parse_params(context(), "gen_stubs=false,plugins=grpc"))
    end

    test "raises on invalid gen_stubs values" do
      assert_raise RuntimeError,
                   ~s(invalid value for gen_stubs option, expected "true" or "false", got: "maybe"),
                   fn ->
                     CLI.parse_params(context(), "gen_stubs=maybe")
                   end
    end
  end

  defp context do
    %Context{
      package: "helloworld",
      dep_type_mapping: %{
        ".helloworld.HelloRequest" => %{type_name: "Helloworld.HelloRequest"},
        ".helloworld.HelloReply" => %{type_name: "Helloworld.HelloReply"}
      }
    }
  end

  defp service do
    %Google.Protobuf.ServiceDescriptorProto{
      name: "Greeter",
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          name: "SayHello",
          input_type: ".helloworld.HelloRequest",
          output_type: ".helloworld.HelloReply",
          options: %Google.Protobuf.MethodOptions{}
        }
      ]
    }
  end
end

defmodule Helloworld do
  
  defmodule Greeter.Service do
    use GRPC.Service, name: "helloworld.Greeter",
                      marshal_function: :encode,
                      unmarshal_function: :decode

    rpc :SayHello, Helloworld.HelloRequest, Helloworld.HelloReply
  end

  defmodule Greeter.Stub do
    use GRPC.Stub, service: Greeter.Service
  end
  
  use Protobuf, """
syntax = "proto3";

option java_multiple_files = true;
option java_package = "io.grpc.examples.helloworld";
option java_outer_classname = "HelloWorldProto";
option objc_class_prefix = "HLW";

package helloworld;

// The greeting service definition.
service Greeter {
  // Sends a greeting
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

// The request message containing the user's name.
message HelloRequest {
  string name = 1;
}

// The response message containing the greetings
message HelloReply {
  string message = 1;
}
  """

end

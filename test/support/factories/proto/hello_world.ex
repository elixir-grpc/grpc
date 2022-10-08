defmodule GRPC.Factories.Proto.HelloWorld do
  defmacro __using__(_opts) do
    quote do
      def hello_reply_rpc_factory do
        %Helloworld.HelloReply{message: "Hello Luis"}
      end
    end
  end
end

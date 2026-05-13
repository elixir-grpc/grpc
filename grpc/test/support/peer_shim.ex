if not Code.ensure_loaded?(:peer) do
  defmodule GRPC.Test.PeerShim do
    @moduledoc """
    Minimal `:peer` compatibility shim for OTP 24, since `:peer` was added in OTP 25.
    """

    def start_link(options) do
      name = Map.fetch!(options, :name)
      [~c"-setcookie", cookie] = Map.fetch!(options, :args)

      ensure_origin_node(cookie)

      {:ok, node} = :slave.start_link(~c"127.0.0.1", name, ~c"-setcookie " ++ cookie)

      {:ok, node, node}
    end

    def call(node, module, function, args), do: :rpc.call(node, module, function, args)

    def random_name(prefix), do: ~c"#{prefix}-#{:erlang.unique_integer([:positive])}"

    def stop(node), do: :slave.stop(node)

    defp ensure_origin_node(cookie) do
      if not Node.alive?() do
        :os.cmd(~c"epmd -daemon")

        {:ok, _} =
          :net_kernel.start([
            :"grpcorigin-#{:os.getpid()}-#{:erlang.unique_integer([:positive])}@127.0.0.1",
            :longnames
          ])
      end

      Node.set_cookie(String.to_atom(to_string(cookie)))
    end
  end
end

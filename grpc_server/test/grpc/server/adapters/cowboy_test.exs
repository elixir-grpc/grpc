defmodule GRPC.Server.Adapters.CowboyTest do
  use ExUnit.Case, async: false

  alias GRPC.Server.Adapters.Cowboy

  describe "child_spec/4" do
    test "produces the correct socket opts for ranch_tcp for inet" do
      spec =
        Cowboy.child_spec(:endpoint, [], 8080, [
          {:adapter_opts,
           [
             {:foo, :bar},
             {:ip, {127, 0, 0, 1}},
             {:ipv6_v6only, false},
             {:net, :inet},
             {:baz, :foo}
           ]}
        ])

      socket_opts = get_socket_opts_from_child_spec(spec)

      assert Enum.sort(socket_opts) ==
               Enum.sort([:inet, {:ip, {127, 0, 0, 1}}, {:ipv6_v6only, false}, {:port, 8080}])
    end

    test "produces the correct socket opts for ranch_tcp for inet6" do
      spec =
        Cowboy.child_spec(:endpoint, [], 8081, [
          {:adapter_opts,
           [
             {:foo, :bar},
             {:ip, {0, 0, 0, 0, 0, 0, 0, 1}},
             {:ipv6_v6only, true},
             {:net, :inet6},
             {:baz, :foo}
           ]}
        ])

      socket_opts = get_socket_opts_from_child_spec(spec)

      assert Enum.sort(socket_opts) ==
               Enum.sort([
                 :inet6,
                 {:ip, {0, 0, 0, 0, 0, 0, 0, 1}},
                 {:ipv6_v6only, true},
                 {:port, 8081}
               ])
    end
  end

  defp get_socket_opts_from_child_spec(spec) do
    {_Cowboy, _start_link, start_opts} = spec.start

    # start_opts is directly the ranch listener args:
    # [ref, transport, trans_opts, protocol, proto_opts]
    # where ref might be a string in newer ranch versions
    [_ref, _ranch_tcp, transport_opts_map, _cowboy_clear, _opts_map] = start_opts
    transport_opts_map.socket_opts
  end
end

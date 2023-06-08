defmodule GRPC.Server.Adapters.CowboyTest do
  use ExUnit.Case, async: false

  alias GRPC.Server.Adapters.Cowboy

  describe "child_spec/4" do
    test "produces the correct socket opts for ranch_tcp for inet" do
      spec =
        Cowboy.child_spec(:endpoint, [], 8080, [
          {:foo, :bar},
          {:ip, {127, 0, 0, 1}},
          {:ipv6_v6only, false},
          {:net, :inet},
          {:baz, :foo}
        ])

      socket_opts = get_socket_opts_from_child_spec(spec)
      assert socket_opts == [:inet, {:ipv6_v6only, false}, {:ip, {127, 0, 0, 1}}, {:port, 8080}]
    end

    test "produces the correct socket opts for ranch_tcp for inet6" do
      spec =
        Cowboy.child_spec(:endpoint, [], 8081, [
          {:foo, :bar},
          {:ip, {0, 0, 0, 0, 0, 0, 0, 1}},
          {:ipv6_v6only, true},
          {:net, :inet6},
          {:baz, :foo}
        ])

      socket_opts = get_socket_opts_from_child_spec(spec)

      assert socket_opts == [
               :inet6,
               {:ipv6_v6only, true},
               {:ip, {0, 0, 0, 0, 0, 0, 0, 1}},
               {:port, 8081}
             ]
    end
  end

  defp get_socket_opts_from_child_spec(spec) do
    {_Cowboy, _start_link, start_opts} = spec.start
    [_http, _endpoint, _empty_list, ranch_listener_call] = start_opts
    {_ranch_listener_sup, _start_link, ranch_listener_opts} = ranch_listener_call
    [_endpoint, _ranch_tcp, transport_opts, _cowboy_clear, _opts_map] = ranch_listener_opts
    transport_opts.socket_opts
  end
end

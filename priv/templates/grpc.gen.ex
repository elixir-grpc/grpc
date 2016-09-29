defmodule <%= top_mod %> do
  <%= if use_proto_path do %>
  @external_resource Path.expand("<%= proto_path %>", __DIR__)
  use Protobuf, from: Path.expand("<%= proto_path %>", __DIR__)
  <% end %>
  <%= Enum.map proto.services, fn(service) -> %>
  defmodule <%= service.name %>.Service do
    use GRPC.Service, name: "<%= service_prefix %><%= service.name %>"

    <%= for rpc <- service.rpcs do %>
    <%= compose_rpc.(rpc, top_mod) %>
    <% end %>
  end

  defmodule <%= service.name %>.Stub do
    use GRPC.Stub, service: <%= service.name %>.Service
  end
  <% end %>
  <%= if !use_proto_path do %>
  use Protobuf, """
  <%= proto_content %>
  """
  <% end %>

end

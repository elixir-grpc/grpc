Code.require_file("./support/test_adapter.exs", __DIR__)

case Application.get_env(:grpc, :http2_client_adapter) do
  GRPC.Adapter.Gun ->
    Application.ensure_all_started(:gun)
  GRPC.Adapter.Chatterbox.Client ->
    Application.ensure_all_started(:chatterbox)
end

ExUnit.start()

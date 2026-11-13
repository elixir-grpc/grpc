defmodule GRPC.Server.TranscodeBindingsContentTypeTest do
  # GHSA-xc5v-89vq-hv5m: path bindings were applied only when the Content-Type chose
  # transcoding. The same URL with `application/grpc` decoded the body alone, so a
  # caller could set a field the path is meant to fix.
  use ExUnit.Case, async: false

  @registered :transcode_bindings_content_type_test

  defmodule TranscodeServer do
    use GRPC.Server, service: Transcode.Messaging.Service, http_transcode: true

    def get_message(req, _stream) do
      send(:transcode_bindings_content_type_test, {:saw, req.name})
      %Transcode.Message{name: req.name, text: "get_message"}
    end
  end

  # A request-streaming RPC with an HTTP rule, which the generated test service lacks.
  defmodule UploadService do
    use GRPC.Service, name: "transcode.Uploads", protoc_gen_elixir_version: "0.15.0"

    rpc(:Upload, stream(Transcode.GetMessageRequest), Transcode.Message, %{
      http: %{
        type: Google.Api.PbExtension,
        value: %Google.Api.HttpRule{
          selector: "",
          body: "*",
          additional_bindings: [],
          response_body: "",
          pattern: {:post, "/v1/uploads/{name}"},
          __unknown_fields__: []
        }
      }
    })
  end

  defmodule UploadServer do
    use GRPC.Server, service: UploadService, http_transcode: true

    def upload(req_enum, _stream) do
      # The JSON codec can hand over a plain map, so read the name from either shape.
      for req <- req_enum do
        send(:transcode_bindings_content_type_test, {:saw, Map.get(req, :name) || req["name"]})
      end

      %Transcode.Message{name: "done", text: "upload"}
    end
  end

  @victim_frame (
                  proto = Protobuf.encode(%Transcode.GetMessageRequest{name: "victim"})
                  <<0::8, byte_size(proto)::32, proto::binary>>
                )

  # {content-type, body} pairs a caller can send to the transcoded URL. A request
  # with no content-type is checked separately.
  @other_content_types [
    {"application/grpc", @victim_frame},
    {"application/grpc+proto", @victim_frame},
    {"application/grpc-web", @victim_frame},
    {"application/grpc-web+proto", @victim_frame},
    {"application/grpc-web-text", Base.encode64(@victim_frame)},
    {"text/plain", @victim_frame}
  ]

  setup do
    Process.register(self(), @registered)
    :ok
  end

  for protocol <- [:http, :http2] do
    describe "over #{protocol}" do
      test "the path binding is used with application/json" do
        run_server(TranscodeServer, fn port ->
          {:ok, conn} = open(port, unquote(protocol))
          ref = :gun.get(conn, "/v1/messages/alice", [{"content-type", "application/json"}])
          assert {:ok, _status, body} = response(conn, ref)

          assert names_seen() == ["alice"]
          assert %{"name" => "alice"} = Jason.decode!(body)
        end)
      end

      for {content_type, body} <- @other_content_types do
        test "a #{content_type} body cannot replace the path binding" do
          run_server(TranscodeServer, fn port ->
            {:ok, conn} = open(port, unquote(protocol))

            ref =
              :gun.request(
                conn,
                "GET",
                "/v1/messages/alice",
                [{"content-type", unquote(content_type)}],
                unquote(body)
              )

            # The body is not JSON, so the call is refused as a bad request.
            assert {:ok, 400, _} = response(conn, ref)
            assert names_seen() -- ["alice"] == []
          end)
        end
      end

      test "a request with no content-type cannot replace the path binding" do
        run_server(TranscodeServer, fn port ->
          {:ok, conn} = open(port, unquote(protocol))
          ref = :gun.request(conn, "GET", "/v1/messages/alice", [], @victim_frame)

          response(conn, ref)
          assert names_seen() -- ["alice"] == []
        end)
      end
    end
  end

  # The `name` of every request the handler received, oldest first.
  test "a request-streaming call to a transcoded URL cannot set the path field" do
    json = Jason.encode!(%{"name" => "victim"})
    json_frame = <<0::8, byte_size(json)::32, json::binary>>

    # The proto frames fit the codec the Content-Type names. The JSON frames fit the
    # codec a transcoded route uses.
    bodies =
      for {content_type, body} <- @other_content_types ++ [{"application/grpc", json_frame}],
          do: {content_type, body}

    for protocol <- [:http, :http2], {content_type, body} <- bodies do
      run_server(UploadServer, fn port ->
        {:ok, conn} = open(port, protocol)
        ref = :gun.post(conn, "/v1/uploads/alice", [{"content-type", content_type}], body)
        assert {:ok, 501, _} = response(conn, ref)

        assert names_seen() -- ["alice"] == [],
               "#{protocol} #{content_type}: the handler saw a name the URL does not give"
      end)
    end
  end

  defp names_seen(acc \\ []) do
    receive do
      {:saw, name} -> names_seen([name | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  defp open(port, protocol) do
    {:ok, conn} = :gun.open(~c"localhost", port, %{protocols: [protocol], retry: 0})
    {:ok, ^protocol} = :gun.await_up(conn, 5_000)
    {:ok, conn}
  end

  # Waits for the whole response. A reply that never comes is not a failure here: the
  # test only cares what the handler saw.
  defp response(conn, ref) do
    case :gun.await(conn, ref, 2_000) do
      {:response, :fin, status, _headers} ->
        {:ok, status, ""}

      {:response, :nofin, status, _headers} ->
        case :gun.await_body(conn, ref, 2_000) do
          {:ok, body} -> {:ok, status, body}
          other -> other
        end

      other ->
        other
    end
  end

  defp run_server(server, func) do
    {:ok, _pid, port} =
      start_supervised(%{
        id: {GRPC.Server, System.unique_integer([:positive])},
        start: {GRPC.Server, :start, [server, 0, []]},
        type: :worker,
        restart: :permanent,
        shutdown: 500
      })

    try do
      func.(port)
    after
      GRPC.Server.stop(server)
    end
  end
end

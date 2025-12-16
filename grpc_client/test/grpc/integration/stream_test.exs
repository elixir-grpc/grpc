defmodule GRPC.StreamTest do
  use GRPC.Integration.TestCase
  doctest GRPC.Stream

  describe "run/1" do
    defmodule MyGRPCService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
        end)
        |> GRPC.Stream.run()
      end
    end

    test "runs a unary stream" do
      run_server([MyGRPCService], fn port ->
        point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        expected_response = %Routeguide.Feature{
          location: point,
          name: "#{point.latitude},#{point.longitude}"
        }

        assert {:ok, response, %{trailers: trailers}} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, point, return_headers: true)

        assert response == expected_response
        assert trailers == GRPC.Transport.HTTP2.server_trailers()
      end)
    end
  end

  describe "map_error/2" do
    defmodule MapErrorService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          # Trigger error when latitude is 0
          if point.latitude == 0 do
            raise "Boom! Invalid latitude"
          end

          %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
        end)
        |> GRPC.Stream.map_error(fn error ->
          case error do
            {:error, {:exception, %{message: msg}}} ->
              {:error,
               GRPC.RPCError.exception(status: :invalid_argument, message: "Error: #{msg}")}

            other ->
              # Not an error, return as-is to continue the flow
              other
          end
        end)
        |> GRPC.Stream.run()
      end
    end

    defmodule DirectRPCErrorService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          # Trigger error when latitude is negative
          if point.latitude < 0 do
            raise "Negative latitude not allowed"
          end

          %Routeguide.Feature{location: point, name: "#{point.latitude},#{point.longitude}"}
        end)
        |> GRPC.Stream.map_error(fn error ->
          case error do
            {:error, {:exception, %{message: msg}}} ->
              # Return RPCError directly without {:error, ...} wrapper
              GRPC.RPCError.exception(status: :out_of_range, message: "Direct error: #{msg}")

            other ->
              # Not an error, return as-is to continue the flow
              other
          end
        end)
        |> GRPC.Stream.run()
      end
    end

    defmodule ExplicitValidationService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          # Trigger different error types based on coordinates
          cond do
            point.latitude == 999 ->
              raise RuntimeError, "Runtime error occurred"

            point.latitude == 888 ->
              raise ArgumentError, "Argument is invalid"

            point.latitude == 777 ->
              raise "Simple string error"

            true ->
              %Routeguide.Feature{location: point, name: "valid"}
          end
        end)
        |> GRPC.Stream.map_error(fn error ->
          # Explicitly validate the error structure
          case error do
            {:error, {:exception, exception_data}} when is_map(exception_data) ->
              # Validate that we have the expected exception structure
              message = Map.get(exception_data, :message)
              kind = Map.get(exception_data, :kind, :error)

              cond do
                is_binary(message) and message =~ "Runtime error" ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :internal,
                     message: "Validated: RuntimeError - #{message}"
                   )}

                is_binary(message) and message =~ "Argument is invalid" ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :invalid_argument,
                     message: "Validated: ArgumentError - #{message}"
                   )}

                is_binary(message) ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :unknown,
                     message: "Validated: #{kind} - #{message}"
                   )}

                true ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :unknown,
                     message: "Validated but no message found"
                   )}
              end

            other ->
              # Not an exception error, pass through
              other
          end
        end)
        |> GRPC.Stream.run()
      end
    end

    defmodule MultipleErrorsService do
      use GRPC.Server, service: Routeguide.RouteGuide.Service

      def get_feature(input, materializer) do
        GRPC.Stream.unary(input, materializer: materializer)
        |> GRPC.Stream.map(fn point ->
          cond do
            point.latitude == 0 ->
              raise "Invalid latitude: cannot be zero"

            point.longitude == 0 ->
              raise "Invalid longitude: cannot be zero"

            point.latitude < 0 ->
              raise ArgumentError, "Latitude must be positive"

            true ->
              %Routeguide.Feature{location: point, name: "valid"}
          end
        end)
        |> GRPC.Stream.map_error(fn error ->
          case error do
            {:error, {:exception, %{message: msg}}} when is_binary(msg) ->
              cond do
                msg =~ "latitude" ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :invalid_argument,
                     message: "Latitude error: #{msg}"
                   )}

                msg =~ "longitude" ->
                  {:error,
                   GRPC.RPCError.exception(
                     status: :invalid_argument,
                     message: "Longitude error: #{msg}"
                   )}

                true ->
                  {:error,
                   GRPC.RPCError.exception(status: :unknown, message: "Unknown error: #{msg}")}
              end

            other ->
              # Not an error we handle, return as-is to continue the flow
              other
          end
        end)
        |> GRPC.Stream.run()
      end
    end

    @tag :map_error
    test "handles errors with map_error and sends RPCError to client" do
      run_server([MapErrorService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test with invalid latitude (0) - should trigger error
        invalid_point = %Routeguide.Point{latitude: 0, longitude: -746_188_906}

        result =
          Routeguide.RouteGuide.Stub.get_feature(channel, invalid_point, return_headers: true)

        # Should receive error response with custom message
        assert {:error, error} = result
        assert %GRPC.RPCError{} = error
        # Status is returned as integer (3 = INVALID_ARGUMENT)
        assert error.status == 3
        assert error.message =~ "Error: Boom! Invalid latitude"
      end)
    end

    @tag :map_error
    test "handles successful requests without triggering map_error" do
      run_server([MapErrorService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test with valid latitude (non-zero) - should succeed
        valid_point = %Routeguide.Point{latitude: 409_146_138, longitude: -746_188_906}

        result =
          Routeguide.RouteGuide.Stub.get_feature(channel, valid_point, return_headers: true)

        assert {:ok, response, _metadata} = result
        assert response.location == valid_point
        assert response.name == "409146138,-746188906"
      end)
    end

    @tag :map_error
    test "handles RPCError returned directly without {:error, ...} wrapper" do
      run_server([DirectRPCErrorService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test with negative latitude - should trigger error
        negative_point = %Routeguide.Point{latitude: -50, longitude: 100}

        result =
          Routeguide.RouteGuide.Stub.get_feature(channel, negative_point, return_headers: true)

        # Should receive error response with custom message
        assert {:error, error} = result
        assert %GRPC.RPCError{} = error
        # Status is returned as integer (11 = OUT_OF_RANGE)
        assert error.status == 11
        assert error.message =~ "Direct error: Negative latitude not allowed"
      end)
    end

    @tag :map_error
    test "handles successful request when using direct RPCError service" do
      run_server([DirectRPCErrorService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test with positive latitude - should succeed
        valid_point = %Routeguide.Point{latitude: 50, longitude: 100}

        result =
          Routeguide.RouteGuide.Stub.get_feature(channel, valid_point, return_headers: true)

        assert {:ok, response, _metadata} = result
        assert response.location == valid_point
        assert response.name == "50,100"
      end)
    end

    @tag :map_error
    test "handles different error types with conditional map_error" do
      run_server([MultipleErrorsService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test latitude error
        lat_error_point = %Routeguide.Point{latitude: 0, longitude: 100}
        assert {:error, error} = Routeguide.RouteGuide.Stub.get_feature(channel, lat_error_point)
        # INVALID_ARGUMENT
        assert error.status == 3
        assert error.message =~ "Latitude error"

        # Test longitude error
        long_error_point = %Routeguide.Point{latitude: 100, longitude: 0}
        assert {:error, error} = Routeguide.RouteGuide.Stub.get_feature(channel, long_error_point)
        # INVALID_ARGUMENT
        assert error.status == 3
        assert error.message =~ "Longitude error"

        # Test ArgumentError (negative latitude) - falls into "Unknown error" branch
        arg_error_point = %Routeguide.Point{latitude: -100, longitude: 100}
        assert {:error, error} = Routeguide.RouteGuide.Stub.get_feature(channel, arg_error_point)
        # UNKNOWN (because message contains "Latitude must be positive")
        assert error.status == 2
        assert error.message =~ "Latitude must be positive"
      end)
    end

    @tag :map_error
    test "explicitly validates exception structure in map_error" do
      run_server([ExplicitValidationService], fn port ->
        {:ok, channel} = GRPC.Stub.connect("localhost:#{port}", adapter_opts: [retry_timeout: 10])

        # Test RuntimeError - should validate and transform to INTERNAL
        runtime_error_point = %Routeguide.Point{latitude: 999, longitude: 100}

        assert {:error, error} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, runtime_error_point)

        # INTERNAL
        assert error.status == 13
        assert error.message =~ "Validated: RuntimeError"
        assert error.message =~ "Runtime error occurred"

        # Test ArgumentError - should validate and transform to INVALID_ARGUMENT
        arg_error_point = %Routeguide.Point{latitude: 888, longitude: 100}
        assert {:error, error} = Routeguide.RouteGuide.Stub.get_feature(channel, arg_error_point)
        # INVALID_ARGUMENT
        assert error.status == 3
        assert error.message =~ "Validated: ArgumentError"
        assert error.message =~ "Argument is invalid"

        # Test simple string error - should validate and transform to UNKNOWN
        string_error_point = %Routeguide.Point{latitude: 777, longitude: 100}

        assert {:error, error} =
                 Routeguide.RouteGuide.Stub.get_feature(channel, string_error_point)

        # UNKNOWN
        assert error.status == 2
        assert error.message =~ "Validated:"
        assert error.message =~ "Simple string error"

        # Test successful request - should not trigger error handling
        valid_point = %Routeguide.Point{latitude: 100, longitude: 100}
        assert {:ok, response} = Routeguide.RouteGuide.Stub.get_feature(channel, valid_point)
        assert response.name == "valid"
      end)
    end
  end
end

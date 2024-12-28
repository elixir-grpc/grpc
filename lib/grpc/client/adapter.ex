defmodule GRPC.Client.Adapter do
  @moduledoc """
  HTTP client adapter for GRPC.
  """

  alias GRPC.Client.Stream
  alias GRPC.Channel

  @typedoc "Determines if the headers have finished being read."
  @type fin :: :fin | :nofin

  @callback connect(channel :: Channel.t(), opts :: keyword()) ::
              {:ok, Channel.t()} | {:error, any()}

  @callback disconnect(channel :: Channel.t()) :: {:ok, Channel.t()} | {:error, any()}

  @callback send_request(stream :: Stream.t(), contents :: iodata(), opts :: keyword()) ::
              Stream.t()

  @doc """
  Check `GRPC.Stub.recv/2` for more context about the return types
  """
  @callback receive_data(stream :: Stream.t(), opts :: keyword()) ::
              GRPC.Stub.receive_data_return() | {:error, any()}

  @doc """
  This callback is used to open a stream connection to the server.
  Mostly used when the payload for this request is streamed.
  To send data using the open stream request, you should use `send_data/3`
  """
  @callback send_headers(stream :: Stream.t(), opts :: keyword()) :: Stream.t()

  @doc """
  This callback will be responsible to send data to the server on a stream
  request is open using `send_headers/2`
   Opts:
      - :send_end_stream (optional) - ends the request stream
  """
  @callback send_data(stream :: Stream.t(), message :: iodata(), opts :: keyword()) :: Stream.t()

  @doc """
  Similarly to the option sent on `send_data/2` - :send_end_stream -
  this callback will end request stream
  """
  @callback end_stream(stream :: Stream.t()) :: Stream.t()

  @doc """
  Cancel a stream in a streaming client.
  """
  @callback cancel(stream :: Stream.t()) :: :ok | {:error, any()}
end

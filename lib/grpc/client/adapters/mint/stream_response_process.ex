defmodule GRPC.Client.Adapters.Mint.StreamResponseProcess do
  @moduledoc """
  This module represents the process responsible for consuming the
    incoming messages from a connection. For each request, there will be
    a process responsible for consuming its messages. At the end of a stream
    this process will automatically be killed.
  """

  @typep accepted_types :: :data | :trailers | :headers | :error
  @typep data_types :: binary() | Mint.Types.headers() | Mint.Types.error()

  @accepted_types [:data, :trailers, :headers, :error]
  @header_types [:headers, :trailers]

  use GenServer

  @spec start_link(GRPC.Client.Stream.t(), send_headers_or_trailers? :: boolean()) ::
          GenServer.on_start()
  def start_link(stream, send_headers_or_trailers?) do
    GenServer.start_link(__MODULE__, {stream, send_headers_or_trailers?})
  end

  @doc """
  Given a pid from this process, build an Elixir.Stream that will consume the accumulated
    data inside this process
  """
  @spec build_stream(pid()) :: Enumerable.t()
  def build_stream(pid) do
    Elixir.Stream.unfold(pid, fn pid ->
      case GenServer.call(pid, :get_response, :infinity) do
        nil -> nil
        response -> {response, pid}
      end
    end)
  end

  @doc """
  Cast a message to process to inform that the stream has finished
    once all messages are produced. This process will automatically
    be killed.
  """
  @spec done(pid()) :: :ok
  def done(pid) do
    GenServer.cast(pid, {:consume_response, :done})
  end

  @doc """
  Consume an incoming data or trailers/headers
  """
  @spec consume(pid(), type :: accepted_types, data :: data_types) :: :ok
  def consume(pid, type, data) when type in @accepted_types do
    GenServer.cast(pid, {:consume_response, {type, data}})
  end

  # Callbacks

  def init({stream, send_headers_or_trailers?}) do
    state = %{
      grpc_stream: stream,
      send_headers_or_trailers: send_headers_or_trailers?,
      buffer: <<>>,
      responses: [],
      done: false,
      from: nil,
      compressor: nil
    }

    {:ok, state}
  end

  def handle_call(:get_response, from, state) do
    {:noreply, put_in(state[:from], from), {:continue, :produce_response}}
  end

  def handle_cast({:consume_response, {:data, data}}, state) do
    %{
      buffer: buffer,
      grpc_stream: %{response_mod: res_mod, codec: codec},
      responses: responses
    } = state

    case GRPC.Message.get_message(buffer <> data, state.compressor) do
      {{_, message}, rest} ->
        # TODO add code here to handle compressor headers
        response = codec.decode(message, res_mod)
        new_responses = [{:ok, response} | responses]
        new_state = %{state | buffer: rest, responses: new_responses}
        {:noreply, new_state, {:continue, :produce_response}}

      _ ->
        new_state = %{state | buffer: buffer <> data}
        {:noreply, new_state, {:continue, :produce_response}}
    end
  end

  def handle_cast(
        {:consume_response, {type, headers}},
        %{send_headers_or_trailers: true, responses: responses} = state
      )
      when type in @header_types do
    state = update_compressor({type, headers}, state)
    new_responses = [get_headers_response(headers, type) | responses]
    {:noreply, %{state | responses: new_responses}, {:continue, :produce_response}}
  end

  def handle_cast(
        {:consume_response, {type, headers}},
        %{send_headers_or_trailers: false, responses: responses} = state
      )
      when type in @header_types do
    state = update_compressor({type, headers}, state)

    with {:error, _rpc_error} = error <- get_headers_response(headers, type) do
      {:noreply, %{state | responses: [error | responses]}, {:continue, :produce_response}}
    else
      _any -> {:noreply, state, {:continue, :produce_response}}
    end
  end

  def handle_cast({:consume_response, {:error, _error} = error}, %{responses: responses} = state) do
    {:noreply, %{state | responses: [error | responses]}, {:continue, :produce_response}}
  end

  def handle_cast({:consume_response, :done}, state) do
    {:noreply, %{state | done: true}, {:continue, :produce_response}}
  end

  def handle_continue(:produce_response, state) do
    case state do
      %{from: nil} ->
        {:noreply, state}

      %{from: from, responses: [], done: true} ->
        GenServer.reply(from, nil)
        {:stop, :normal, state}

      %{responses: []} ->
        {:noreply, state}

      %{responses: [response | rest], from: from} ->
        GenServer.reply(from, response)
        {:noreply, %{state | responses: rest, from: nil}}
    end
  end

  defp get_headers_response(headers, type) do
    decoded_trailers = GRPC.Transport.HTTP2.decode_headers(headers)
    status = String.to_integer(decoded_trailers["grpc-status"] || "0")

    if status == GRPC.Status.ok() do
      {type, decoded_trailers}
    else
      rpc_error = %GRPC.RPCError{status: status, message: decoded_trailers["grpc-message"]}
      {:error, rpc_error}
    end
  end

  defp update_compressor({:headers, headers}, state) do
    decoded_trailers = GRPC.Transport.HTTP2.decode_headers(headers)

    compressor =
      get_compressor(decoded_trailers["grpc-encoding"], state.grpc_stream.accepted_compressors)

    %{state | compressor: compressor}
  end

  defp update_compressor(_headers, state), do: state

  defp get_compressor(nil = _encoding_name, _accepted_compressors), do: nil

  defp get_compressor(encoding_name, accepted_compressors) do
    Enum.find(accepted_compressors, nil, fn c -> c.name() == encoding_name end)
  end

  def terminate(_reason, _state) do
    :normal
  end
end

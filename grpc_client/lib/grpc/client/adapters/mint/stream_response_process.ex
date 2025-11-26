defmodule GRPC.Client.Adapters.Mint.StreamResponseProcess do
  @moduledoc false
  # This module represents the process responsible for consuming the
  # incoming messages from a connection. For each request, there will be
  # a process responsible for consuming its messages. At the end of a stream
  # this process will automatically be killed.

  # TODO: Refactor the GenServer.call/3 occurrences on this module to produce
  # telemetry events and log entries in case of failures

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
  @spec build_stream(pid(), produce_trailers? :: boolean) :: Enumerable.t()
  def build_stream(pid, produce_trailers? \\ true) do
    Stream.unfold(pid, fn pid ->
      pid
      |> GenServer.call(:get_response, :infinity)
      |> process_response(produce_trailers?, pid)
    end)
  end

  defp process_response(nil = _response, _produce_trailers, _pid), do: nil

  defp process_response({:trailers, _trailers}, false = produce_trailers?, pid) do
    pid
    |> GenServer.call(:get_response, :infinity)
    |> process_response(produce_trailers?, pid)
  end

  defp process_response(response, _produce_trailers, pid) do
    {response, pid}
  end

  @doc """
  Cast a message to process to inform that the stream has finished
    once all messages are produced. This process will automatically
    be killed.
  """
  @spec done(pid()) :: :ok
  def done(pid) do
    :ok = GenServer.call(pid, {:consume_response, :done})
    :ok
  end

  @doc """
  Consume an incoming data or trailers/headers
  """
  @spec consume(pid(), type :: accepted_types, data :: data_types) :: :ok
  def consume(pid, type, data) when type in @accepted_types do
    :ok = GenServer.call(pid, {:consume_response, {type, data}})
    :ok
  end

  # Callbacks

  @impl true
  def init({stream, send_headers_or_trailers?}) do
    state = %{
      grpc_stream: stream,
      send_headers_or_trailers: send_headers_or_trailers?,
      buffer: <<>>,
      responses: :queue.new(),
      done: false,
      from: nil,
      compressor: nil
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_response, from, state) do
    {:noreply, put_in(state[:from], from), {:continue, :produce_response}}
  end

  def handle_call({:consume_response, {:data, data}}, _from, state) do
    %{
      buffer: buffer,
      grpc_stream: %{response_mod: res_mod, codec: codec},
      responses: responses
    } = state

    case GRPC.Message.get_message(buffer <> data, state.compressor) do
      {{_, message}, rest} ->
        # TODO add code here to handle compressor headers
        response = codec.decode(message, res_mod)
        new_responses = :queue.in({:ok, response}, responses)
        new_state = %{state | buffer: rest, responses: new_responses}
        {:reply, :ok, new_state, {:continue, :produce_response}}

      _ ->
        new_state = %{state | buffer: buffer <> data}
        {:reply, :ok, new_state, {:continue, :produce_response}}
    end
  end

  def handle_call(
        {:consume_response, {type, headers}},
        _from,
        %{send_headers_or_trailers: true, responses: responses} = state
      )
      when type in @header_types do
    state = update_compressor({type, headers}, state)
    new_responses = :queue.in(get_headers_response(headers, type), responses)
    {:reply, :ok, %{state | responses: new_responses}, {:continue, :produce_response}}
  end

  def handle_call(
        {:consume_response, {type, headers}},
        _from,
        %{send_headers_or_trailers: false, responses: responses} = state
      )
      when type in @header_types do
    state = update_compressor({type, headers}, state)

    case get_headers_response(headers, type) do
      {:error, _rpc_error} = error ->
        {:reply, :ok, %{state | responses: :queue.in(error, responses)},
         {:continue, :produce_response}}

      _any ->
        {:reply, :ok, state, {:continue, :produce_response}}
    end
  end

  def handle_call(
        {:consume_response, {:error, _error} = error},
        _from,
        %{responses: responses} = state
      ) do
    {:reply, :ok, %{state | responses: :queue.in(error, responses)},
     {:continue, :produce_response}}
  end

  def handle_call({:consume_response, :done}, _from, state) do
    {:reply, :ok, %{state | done: true}, {:continue, :produce_response}}
  end

  @impl true
  def handle_continue(:produce_response, state) do
    no_responses? = :queue.is_empty(state.responses)
    without_from? = is_nil(state.from)

    cond do
      without_from? ->
        {:noreply, state}

      no_responses? and state.done ->
        GenServer.reply(state.from, nil)
        {:stop, :normal, state}

      no_responses? ->
        {:noreply, state}

      true ->
        {{:value, response}, rest} = :queue.out(state.responses)
        GenServer.reply(state.from, response)
        {:noreply, %{state | responses: rest, from: nil}}
    end
  end

  defp get_headers_response(headers, type) do
    decoded_trailers = GRPC.Transport.HTTP2.decode_headers(headers)
    status = String.to_integer(decoded_trailers["grpc-status"] || "0")

    if status == GRPC.Status.ok() do
      {type, decoded_trailers}
    else
      rpc_error =
        GRPC.RPCError.from_grpc_status_details_bin(%{
          status: status,
          message: decoded_trailers["grpc-message"],
          encoded_details_bin: decoded_trailers["grpc-status-details-bin"]
        })

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

  @impl true
  def terminate(_reason, _state) do
    :normal
  end
end

defmodule GRPC.Client.Adapters.Mint.StreamResponseProcess do
  use GenServer

  def start_link(stream, send_trailers?) do
    GenServer.start_link(__MODULE__, {stream, send_trailers?})
  end

  @doc """
  Given a pid, builds a Stream that will consume the accumulated
    data inside this process
  """
  @spec build_stream(pid()) :: Elixir.Stream.t()
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
  """
  @spec done(pid()) :: :ok
  def done(pid) do
    GenServer.cast(pid, {:consume_response, :done})
  end

  @doc """
  Cast a message to process to consume an incoming data or trailers
  """
  @spec consume(pid(), :data | :trailers, binary() | Mint.Types.headers()) :: :ok
  def consume(pid, :data, data) do
    GenServer.cast(pid, {:consume_response, {:data, data}})
  end

  def consume(pid, :trailers, trailers) do
    GenServer.cast(pid, {:consume_response, {:trailers, trailers}})
  end

  # Callbacks

  def init({stream, send_trailers?}) do
    state = %{
      grpc_stream: stream,
      send_trailers: send_trailers?,
      buffer: <<>>,
      responses: [],
      done: false,
      from: nil
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

    case GRPC.Message.get_message(buffer <> data) do
      {{_, message}, rest} ->
        response = codec.decode(message, res_mod)
        new_responses = [{:ok, response} | responses]
        new_state = %{state | buffer: rest, responses: new_responses}
        {:noreply, new_state, {:continue, :produce_response}}

      _ ->
        new_state = %{state | bufferr: buffer <> data}
        {:noreply, new_state, {:continue, :produce_response}}
    end
  end

  def handle_cast(
        {:consume_response, {:trailers, trailers}},
        %{send_trailers: true, responses: responses} = state
      ) do
    new_responses = [get_trailers_response(trailers) | responses]
    {:noreply, %{state | responses: new_responses}, {:continue, :produce_response}}
  end

  def handle_cast(
        {:consume_response, {:trailers, trailers}},
        %{send_trailers: false, responses: responses} = state
      ) do
    with {:errors, _rpc_error} = error <- get_trailers_response(trailers) do
      {:noreply, %{state | responses: [error | responses]}, {:continue, :produce_response}}
    else
      _any -> {:noreply, state, {:continue, :produce_response}}
    end
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

  defp get_trailers_response(trailers) do
    decoded_trailers = GRPC.Transport.HTTP2.decode_headers(trailers)
    status = String.to_integer(decoded_trailers["grpc-status"])

    case status == GRPC.Status.ok() do
      true ->
        {:trailers, decoded_trailers}

      false ->
        rpc_error = %GRPC.RPCError{status: status, message: decoded_trailers["grpc-message"]}
        {:error, rpc_error}
    end
  end

  def terminate(_reason, _state) do
    :normal
  end
end

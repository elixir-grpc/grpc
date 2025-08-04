defmodule Grpc.Client.Adapters.Finch.RequestProcess do
  use GenServer

  alias Grpc.Client.Adapters.Finch.StreamRequestProcess

  @finch_instance_name Finch.GRPC

  def start_link(stream_request_pid, path, client_headers, data \\ nil) do
    GenServer.start_link(__MODULE__, [stream_request_pid, path, client_headers, data])
  end

  @impl true
  def init([stream_request_pid, path, client_headers, data]) do
    req = Finch.build(:post, path, client_headers, data)

    stream_ref = Finch.async_request(req, @finch_instance_name)

    Process.monitor(stream_request_pid)

    {:ok,
     %{
       stream_ref: stream_ref,
       stream_request_pid: stream_request_pid,
       recieved_headers: false,
       responses: :queue.new(),
       from: nil
     }}
  end

  @impl true
  def handle_info({ref, {:status, 200}}, %{stream_ref: ref} = state) do
    IO.inspect(:status, label: __MODULE__)
    {:noreply, state}
  end

  @impl true
  def handle_info({ref, {:headers, headers}}, %{stream_ref: ref} = state) do
    IO.inspect(:headers, label: __MODULE__)

    msg =
      if state.recieved_headers do
        {:trailers, headers}
      else
        {:headers, headers}
      end

    StreamRequestProcess.consume(state.stream_request_pid, msg)
    {:noreply, %{state | recieved_headers: true}}
  end

  @impl true
  def handle_info({ref, {:data, data}}, %{stream_ref: ref} = state) do
    IO.inspect(:data, label: __MODULE__)
    StreamRequestProcess.consume(state.stream_request_pid, {:data, data})
    {:noreply, state}
  end

  @impl true
  def handle_info({ref, {:error, exception}}, %{stream_ref: ref} = state) do
    IO.inspect(exception, label: __MODULE__)
    StreamRequestProcess.consume(state.stream_request_pid, {:error, exception})
    {:stop, :normal, state}
  end

  @impl true
  def handle_info({ref, :done}, %{stream_ref: ref} = state) do
    IO.inspect(:done, label: __MODULE__)
    StreamRequestProcess.consume(state.stream_request_pid, :done)
    {:stop, :normal, state}
  end
end

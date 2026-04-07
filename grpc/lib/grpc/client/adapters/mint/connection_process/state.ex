defmodule GRPC.Client.Adapters.Mint.ConnectionProcess.State do
  @moduledoc false

  defstruct [:conn, :parent, requests: %{}, request_stream_queue: :queue.new()]

  @type t :: %__MODULE__{
          conn: Mint.HTTP.t(),
          requests: map(),
          parent: pid()
        }

  def new(conn, parent) do
    %__MODULE__{conn: conn, request_stream_queue: :queue.new(), parent: parent}
  end

  def update_conn(state, conn) do
    %{state | conn: conn}
  end

  def update_request_stream_queue(state, queue) do
    %{state | request_stream_queue: queue}
  end

  def put_empty_ref_state(state, ref, response_pid) do
    put_in(state.requests[ref], %{
      stream_response_pid: response_pid,
      done: false,
      response: %{}
    })
  end

  def update_response_status(state, ref, status) do
    put_in(state.requests[ref].response[:status], status)
  end

  def update_response_headers(state, ref, headers) do
    put_in(state.requests[ref].response[:headers], headers)
  end

  def empty_headers?(state, ref) do
    is_nil(state.requests[ref].response[:headers])
  end

  def stream_response_pid(state, ref) do
    state.requests[ref].stream_response_pid
  end

  def pop_ref(state, ref) do
    pop_in(state.requests[ref])
  end

  def append_response_data(state, ref, new_data) do
    update_in(state.requests[ref].response[:data], fn data -> (data || "") <> new_data end)
  end
end

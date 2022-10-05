defmodule GRPC.Client.Adapters.Mint.ConnectionProcess.State do
  defstruct [:conn, requests: %{}, request_stream_queue: :queue.new()]

  @type t :: %__MODULE__{
          conn: Mint.HTTP.t(),
          requests: map()
        }

  def new(conn) do
    %__MODULE__{conn: conn, request_stream_queue: :queue.new()}
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
      response: %{},
      request: %{from: nil, stream_queue: []}
    })
  end

  def put_in_ref(state, ref, ref_state) do
    put_in(state.requests[ref], ref_state)
  end

  def update_response_status(state, ref, status) do
    put_in(state.requests[ref].response[:status], status)
  end

  def update_response_headers(state, ref, headers) do
    put_in(state.requests[ref].response[:headers], headers)
  end

  def update_response_trailers(state, ref, trailers) do
    put_in(state.requests[ref].response[:trailers], trailers)
  end

  def steamed_response?(state, ref) do
    state.requests[ref].streamed_response
  end

  def empty_headers?(state, ref) do
    is_nil(state.requests[ref].response[:headers])
  end

  def stream_response_pid(state, ref) do
    state.requests[ref].stream_response_pid
  end

  def caller_process(state, ref) do
    state.requests[ref][:from]
  end

  def get_response(state, ref) do
    state.requests[ref][:response]
  end

  def get_request_stream_by_ref(state, ref) do
    state.requests[ref][:request]
  end

  def pop_ref(state, ref) do
    pop_in(state.requests[ref])
  end

  def append_response_data(state, ref, new_data) do
    update_in(state.requests[ref].response[:data], fn data -> (data || "") <> new_data end)
  end

  def enqueue_request_to_stream(state, ref, body) do
    update_in(state.requests[ref].request[:stream_queue], fn queue -> queue ++ [body] end)
  end

  def stream_queue_not_empty?(state, ref) do
    state.requests[ref].request[:stream_queue] != []
  end

  def put_request_back_to_stream_queue(state, ref, body) do
    update_in(state.requests[ref].request[:stream_queue], fn queue -> [body | queue] end)
  end

  def update_request_reference(state, ref, from) do
    put_in(state.requests[ref].request[:from], from)
  end

  def update_request_queue(state, ref, queue) do
    put_in(state.requests[ref].request[:stream_queue], queue)
  end

  def update_request(state, ref, request) do
    put_in(state.requests[ref].request, request)
  end
end

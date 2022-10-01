defmodule GRPC.Client.Adapters.Mint.ConnectionProcess.State do
  defstruct [:conn, requests: %{}]

  @type t :: %__MODULE__{
          conn: Mint.HTTP.t(),
          requests: map()
        }

  def new(conn) do
    %__MODULE__{conn: conn}
  end

  def update_conn(state, conn) do
    %{state | conn: conn}
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

  def pop_ref(state, ref) do
    pop_in(state.requests[ref])
  end

  def append_response_data(state, ref, new_data) do
    update_in(state.requests[ref].response[:data], fn data -> (data || "") <> new_data end)
  end
end

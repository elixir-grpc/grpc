if Code.ensure_loaded?(Mint.HTTP) do
  defmodule GRPC.Client.Adapters.Mint.ConnectionProcess.State do
    @moduledoc false

    defstruct [
      :conn,
      :parent,
      :scheme,
      :host,
      :port,
      :connect_opts,
      :retry_timeout_ms,
      requests: %{},
      request_stream_queue: :queue.new(),
      retry: 0,
      retry_attempt: 0
    ]

    def new(conn, opts) do
      %__MODULE__{
        conn: conn,
        request_stream_queue: :queue.new(),
        parent: opts[:parent],
        scheme: opts[:scheme],
        host: opts[:host],
        port: opts[:port],
        connect_opts: opts[:connect_opts] || [],
        retry: opts[:retry] || 0
      }
    end

    def update_conn(state, conn) do
      %{state | conn: conn}
    end

    def update_request_stream_queue(state, queue) do
      %{state | request_stream_queue: queue}
    end

    def put_empty_ref_state(state, ref, response_pid, monitor_ref \\ nil) do
      put_in(state.requests[ref], %{
        stream_response_pid: response_pid,
        monitor_ref: monitor_ref,
        done: false,
        response: %{}
      })
    end

    def find_request_ref_by_monitor_ref(state, monitor_ref) do
      Enum.find_value(state.requests, fn {ref, req} ->
        if req[:monitor_ref] == monitor_ref, do: ref
      end)
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

    defguard has_request_ref(state, ref) when is_map_key(state.requests, ref)

    def pop_ref(state, ref) do
      {request_data, new_requests} = Map.pop(state.requests, ref)

      if is_map(request_data) and is_reference(request_data[:monitor_ref]) do
        Process.demonitor(request_data[:monitor_ref], [:flush])
      end

      {request_data, %{state | requests: new_requests}}
    end

    def append_response_data(state, ref, new_data) do
      update_in(state.requests[ref].response[:data], fn data -> (data || "") <> new_data end)
    end
  end
end

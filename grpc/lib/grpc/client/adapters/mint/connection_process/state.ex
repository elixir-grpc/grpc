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
      :telemetry_metadata,
      requests: %{},
      request_monitors: %{},
      request_stream_queue: :queue.new(),
      retry: 0,
      retry_attempt: 0,
      generation: 1,
      max_concurrent_streams: :unknown,
      settings_known?: false
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
        retry: opts[:retry] || 0,
        telemetry_metadata: opts[:telemetry_metadata]
      }
    end

    def update_conn(state, conn) do
      %{state | conn: conn}
    end

    def update_request_stream_queue(state, queue) do
      %{state | request_stream_queue: queue}
    end

    def put_empty_ref_state(state, ref, response_pid) do
      monitor_ref = Process.monitor(response_pid)

      state
      |> put_in([Access.key(:requests), ref], %{
        stream_response_pid: response_pid,
        done: false,
        response: %{}
      })
      |> put_in([Access.key(:request_monitors), monitor_ref], ref)
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
      {request, state} = pop_in(state.requests[ref])

      case Enum.find(state.request_monitors, fn {_monitor_ref, request_ref} ->
             request_ref == ref
           end) do
        {monitor_ref, ^ref} ->
          Process.demonitor(monitor_ref, [:flush])
          {request, %{state | request_monitors: Map.delete(state.request_monitors, monitor_ref)}}

        nil ->
          {request, state}
      end
    end

    def request_ref_by_monitor(state, monitor_ref) do
      Map.get(state.request_monitors, monitor_ref)
    end

    def append_response_data(state, ref, new_data) do
      update_in(state.requests[ref].response[:data], fn data -> (data || "") <> new_data end)
    end
  end
end

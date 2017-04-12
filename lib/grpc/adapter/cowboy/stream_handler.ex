defmodule GRPC.Adapter.Cowboy.StreamHandler do
  @moduledoc """
  Custom steam handler for cowboy.

  This module is based on default handler `:cowboy_stream_h` of Cowboy,
  which stores the response body as a whole binary so that it's
  impossible to fetch the DATA frames one by one.
  """

  @type stream_id :: non_neg_integer
  @type is_fin :: :nofin | {:fin, non_neg_integer}
  @type t :: %__MODULE__{ref: :ranch.ref, pid: pid, read_body_ref: reference,
                         read_body_timer_ref: reference, read_bodies: list,
                         read_body_is_fin: is_fin}
  defstruct ref: nil, pid: nil, read_body_ref: nil,
            read_body_timer_ref: nil, read_body_is_fin: :nofin, read_bodies: []

  alias __MODULE__, as: State

  @spec init(stream_id, any, keyword) :: {any, t}
  def init(_stream_id, %{ref: ref} = req, opts) do
    env = Map.get(opts, :env, %{})
    middlewares = Map.get(opts, :middlewares, [:cowboy_router, :cowboy_handler])
    shutdown = Map.get(opts, :shutdown, 5000)
    pid = :proc_lib.spawn_link(__MODULE__, :proc_lib_hack, [req, env, middlewares])
    {[{:spawn, pid, shutdown}], %State{ref: ref, pid: pid}}
  end

  # Stream is not waiting for data
  @spec data(stream_id, is_fin, binary, t) :: {list, t}
  def data(_stream_id, is_fin, data, %State{read_body_ref: nil, read_bodies: bodies} = state) do
    {[], %State{state | read_body_is_fin: is_fin, read_bodies: bodies ++ [data]}}
  end
  # Stream is waiting for data, DATA receiving is fin and all bodies are read
  def data(_stream_id, is_fin, data, %State{pid: pid, read_body_ref: ref,
            read_body_timer_ref: t_ref, read_bodies: []} = state) when elem(is_fin, 0) == :fin do
    :ok = :erlang.cancel_timer(t_ref, async: true, info: false)
    send pid, {:request_body, ref, is_fin, data}
    {[], %State{state | read_body_ref: nil, read_body_timer_ref: nil, read_body_is_fin: is_fin, read_bodies: []}}
  end
  def data(_stream_id, is_fin, data, %State{pid: pid, read_body_ref: ref, read_body_timer_ref: t_ref} = state) do
    :ok = :erlang.cancel_timer(t_ref, async: true, info: false)
    send pid, {:request_body, ref, :nofin, data}
    {[], %State{state | read_body_ref: nil, read_body_timer_ref: nil, read_body_is_fin: is_fin}}
  end

  @spec info(stream_id, tuple, t) :: {list, t}
  def info(_stream_id, {:EXIT, pid, :normal}, %State{pid: pid} = state) do
    {[:stop], state}
  end
  def info(_stream_id, {:EXIT, pid, {_reason, [_, {:cow_http_hd, _, _, _}|_]}}, %State{pid: pid} = state) do
    {[{:error_response, 400, %{}, ""}, :stop], state}
  end
  def info(stream_id, {:EXIT, pid, {reason, stacktrace}} = exit, %State{ref: ref, pid: pid} = state) do
    report_crash(ref, stream_id, pid, reason, stacktrace)
    {[
      {:error_response, 500, %{"content-length" => "0"}, ""},
      {:internal_error, exit, :"Stream process crashed."}
    ], state}
  end

  # DATAs receiving is finished and only one data is left
  def info(_stream_id, {:read_body, ref, _length, _},
            %State{pid: pid, read_body_is_fin: is_fin, read_bodies: [data|[]]} = state) when elem(is_fin, 0) == :fin do
    send pid, {:request_body, ref, is_fin, data}
    {[], %State{state | read_bodies: []}}
  end
  # Not all bodies are read
  def info(_stream_id, {:read_body, ref, _length, _}, %State{pid: pid, read_bodies: [data|bodies]} = state) do
    send pid, {:request_body, ref, :nofin, data}
    {[], %State{state | read_bodies: bodies}}
  end
  # DATAs receiving is not finished but bodies are all read, set timeout
  def info(stream_id, {:read_body, ref, length, period},
            %State{read_body_is_fin: :nofin, read_bodies: []} = state) do
    t_ref = :erlang.send_after(period, self(), {{self(), stream_id}, {:read_body_timeout, ref}})
    {[flow: length], %State{state | read_body_ref: ref, read_body_timer_ref: t_ref}}
  end
  # ref is matched
  def info(_stream_id, {:read_body_timeout, ref}, %State{read_body_ref: ref} = state) do
    {[], %State{state | read_body_ref: nil, read_body_timer_ref: nil}}
  end
  # ref is not matched
  def info(_stream_id, {:read_body_timeout, _}, state) do
    {[], state}
  end
  # response
  def info(_stream_id, {:response, _, _, _} = response, state) do
    {[response], state}
  end
  def info(_stream_id, {:headers, _, _} = headers, state) do
    {[headers], state}
  end
  def info(_stream_id, {:trailers, _} = trailers, state) do
    {[trailers], state}
  end
  def info(_stream_id, {:data, _, _} = data, state) do
    {[data], state}
  end
  def info(_stream_id, {:push, _, _, _, _, _, _, _} = push, state) do
    {[push], state}
  end
  def info(_stream_id, {:switch_protocol, _, _, _} = switch_protocol, state) do
    {[switch_protocol], state}
  end
  # stray message
  def info(_stream_id, _msg, state) do
    {[], state}
  end

  @doc false
  @spec terminate(stream_id, any, t) :: :ok
  def terminate(_stream_id, _reason, _state), do: :ok

  defp report_crash(_, _, _, :normal, _), do: :ok
  defp report_crash(_, _, _, :shutdown, _), do: :ok
  defp report_crash(_, _, _, {:shutdown, _}, _), do: :ok
  defp report_crash(ref, stream_id, pid, reason, stacktrace) do
    :error_logger.error_msg(
      "Ranch listener ~p, connection process ~p, stream ~p " <>
      "had its request process ~p exit with reason " <>
      "~999999p and stacktrace ~999999p~n",
      [ref, self(), stream_id, pid, reason, stacktrace])
  end

  @doc false
  @spec proc_lib_hack(any, any, list) :: any
  def proc_lib_hack(req, env, middlewares) do
    try do
      execute(req, env, middlewares)
    catch
      _, reason when elem(reason, 0) == :cowboy_handler ->
        exit(reason)
      _, reason ->
        exit({reason, :erlang.get_stacktrace()})
    end
  end

  @doc false
  @spec execute(any, any, list) :: any
  def execute(_, _, []), do: :ok
  def execute(req, env, [middleware|tail]) do
    res = middleware.execute(req, env)
    case res do
      {:ok, req2, env2} ->
        execute(req2, env2, tail)
      {:suspend, module, function, args} ->
        :proc_lib.hibernate(__MODULE__, :resume, [env, tail, module, function, args])
      {:stop, _req} ->
        :ok
    end
  end

  @doc false
  @spec resume(any, list, atom, any, any) :: any
  def resume(env, tail, module, function, args) do
    case apply(module, function, args) do
      {:ok, req, env} ->
        execute(req, env, tail)
      {:suspend, module2, function2, args2} ->
        :proc_lib.hibernate(__MODULE__, :resume, [env, tail, module2, function2, args2])
      {:stop, _req} ->
        :ok
    end
  end
end

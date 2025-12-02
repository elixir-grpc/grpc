defmodule GRPC.Server.Adapters.Cowboy.Router do
  # Most of the functionality in this module is lifted from :cowboy_router, with the unused parts
  # removed. Since the template language for Google.Api.HttpRule is quite rich, it cannot be expressed
  # in terms of the default routing offered by cowboy.
  # This module is configured to be used as middleware in `src/grpc_stream_h.erl` instead of :cowoby_router
  @moduledoc false
  @behaviour :cowboy_middleware

  alias GRPC.Server.Router

  def compile(routes) do
    for {host, paths} <- routes do
      [{host_match, _, _}] = :cowboy_router.compile([{host, []}])
      compiled_paths = compile_paths(paths, [])

      {host_match, [], compiled_paths}
    end
  end

  def compile_paths([], acc) do
    Enum.reverse(acc)
  end

  def compile_paths([{path, handler, opts} | paths], acc) when is_binary(path) do
    {_, _, matches} = Router.build_route(path)

    compile_paths(paths, [{matches, [], handler, opts} | acc])
  end

  def compile_paths([{route, handler, opts} | paths], acc) do
    compile_paths(paths, [{route, [], handler, opts} | acc])
  end

  @impl :cowboy_middleware
  def execute(
        req = %{host: host, path: path},
        env = %{dispatch: dispatch}
      ) do
    dispatch =
      case dispatch do
        {:persistent_term, key} ->
          :persistent_term.get(key)

        _ ->
          dispatch
      end

    case match(dispatch, host, path) do
      {:ok, handler, handler_opts, bindings, host_info, path_info} ->
        {:ok, Map.merge(req, %{host_info: host_info, path_info: path_info, bindings: bindings}),
         Map.merge(env, %{handler: handler, handler_opts: handler_opts})}

      {:error, :notfound, :host} ->
        {:stop, :cowboy_req.reply(400, req)}

      {:error, :badrequest, :path} ->
        {:stop, :cowboy_req.reply(400, req)}

      {:error, :notfound, :path} ->
        {:stop, :cowboy_req.reply(404, req)}
    end
  end

  def match([], _, _) do
    {:error, :notfound, :host}
  end

  def match([{:_, [], path_matchs} | _Tail], _, path) do
    match_path(path_matchs, :undefined, path, %{})
  end

  defp match_path([], _, _, _) do
    {:error, :notfound, :path}
  end

  defp match_path([{:_, [], handler, opts} | _Tail], host_info, _, bindings) do
    {:ok, handler, opts, bindings, host_info, :undefined}
  end

  defp match_path([{"*", _, handler, opts} | _Tail], host_info, "*", bindings) do
    {:ok, handler, opts, bindings, host_info, :undefined}
  end

  defp match_path([_ | tail], host_info, "*", bindings) do
    match_path(tail, host_info, "*", bindings)
  end

  defp match_path([{path_match, fields, handler, opts} | tail], host_info, tokens, bindings)
       when is_list(tokens) do
    case Router.match(tokens, path_match, bindings) do
      false ->
        match_path(tail, host_info, tokens, bindings)

      {true, path_binds} ->
        case check_constraints(fields, path_binds) do
          {:ok, path_binds} ->
            {:ok, handler, opts, path_binds, host_info, :undefined}

          :nomatch ->
            match_path(tail, host_info, tokens, bindings)
        end
    end
  end

  defp match_path(_Dispatch, _HostInfo, :badrequest, _Bindings) do
    {:error, :badrequest, :path}
  end

  defp match_path(dispatch, host_info, path, bindings) do
    match_path(dispatch, host_info, Router.split_path(path), bindings)
  end

  defp check_constraints([], bindings) do
    {:ok, bindings}
  end

  defp check_constraints([field | tail], bindings) when is_atom(field) do
    check_constraints(tail, bindings)
  end

  defp check_constraints([field | tail], bindings) do
    name = elem(field, 0)

    case bindings do
      %{^name => value} ->
        constraints = elem(field, 1)

        case :cowboy_constraints.validate(
               value,
               constraints
             ) do
          {:ok, value} ->
            check_constraints(tail, Map.put(bindings, name, value))

          {:error, _} ->
            :nomatch
        end

      _ ->
        check_constraints(tail, bindings)
    end
  end
end

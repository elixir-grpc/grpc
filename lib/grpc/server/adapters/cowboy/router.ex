defmodule GRPC.Server.Adapters.Cowboy.Router do
  use Bitwise
  @behaviour :cowboy_middleware

  @dialyzer {:nowarn_function, compile: 1}

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

  def compile_paths([{route, handler, opts} | paths], acc) when is_binary(route) do
    {_, route} = GRPC.Server.Transcode.build_route(%{pattern: {:post, route}})

    compile_paths(paths, [{route, [], handler, opts} | acc])
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

  def match([{host_match, fields, path_matchs} | tail], tokens, path)
      when is_list(tokens) do
    case list_match(tokens, host_match, %{}) do
      false ->
        match(tail, tokens, path)

      {true, bindings, host_info} ->
        host_info =
          case host_info do
            :undefined ->
              :undefined

            _ ->
              Enum.reverse(host_info)
          end

        case check_constraints(fields, bindings) do
          {:ok, bindings} ->
            match_path(path_matchs, host_info, path, bindings)

          :nomatch ->
            match(tail, tokens, path)
        end
    end
  end

  def match(dispatch, host, path) do
    match(dispatch, split_host(host), path)
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
    case list_match(tokens, path_match, bindings) do
      false ->
        match_path(tail, host_info, tokens, bindings)

      {true, path_binds, path_info} ->
        case check_constraints(fields, path_binds) do
          {:ok, path_binds} ->
            {:ok, handler, opts, path_binds, host_info, path_info}

          :nomatch ->
            match_path(tail, host_info, tokens, bindings)
        end
    end
  end

  defp match_path(_Dispatch, _HostInfo, :badrequest, _Bindings) do
    {:error, :badrequest, :path}
  end

  defp match_path(dispatch, host_info, path, bindings) do
    match_path(dispatch, host_info, split_path(path), bindings)
  end

  defp check_constraints([], bindings) do
    {:ok, bindings}
  end

  defp check_constraints([field | tail], bindings) when is_atom(field) do
    check_constraints(tail, bindings)
  end

  defp check_constraints([field | tail], bindings) do
    name = :erlang.element(1, field)

    case bindings do
      %{^name => value} ->
        constraints = :erlang.element(2, field)

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

  defp split_host(host) do
    split_host(host, [])
  end

  defp split_host(host, acc) do
    case :binary.match(host, ".") do
      :nomatch when host === <<>> ->
        acc

      :nomatch ->
        [host | acc]

      {pos, _} ->
        <<segment::size(pos)-binary, _::size(8), rest::bits>> = host
        false = byte_size(segment) == 0
        split_host(rest, [segment | acc])
    end
  end

  defp split_path(<<?/, path::bits>>) do
    split_path(path, [])
  end

  defp split_path(_) do
    :badrequest
  end

  defp split_path(path, acc) do
    try do
      case :binary.match(path, "/") do
        :nomatch when path === <<>> ->
            acc
            |> Enum.map(&:cow_uri.urldecode/1)
            |> Enum.reverse()
            |> remove_dot_segments([])

        :nomatch ->
          [path | acc]
          |> Enum.map(&:cow_uri.urldecode/1)
          |> Enum.reverse()
          |> remove_dot_segments([])

        {pos, _} ->
          <<segment::size(pos)-binary, _::size(8), rest::bits>> = path
          split_path(rest, [segment | acc])
      end
    catch
      :error, _ ->
        :badrequest
    end
  end

  defp remove_dot_segments([], acc) do
    Enum.reverse(acc)
  end

  defp remove_dot_segments(["." | segments], acc) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([".." | segments], acc = []) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([".." | segments], [_ | acc]) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([s | segments], acc) do
    remove_dot_segments(segments, [s | acc])
  end

  def list_match(list, [{:__, []}], binds) do
    {true, binds, list}
  end

  def list_match([_s | tail], [{:_, _} | tail_match], binds) do
    list_match(tail, tail_match, binds)
  end

  def list_match([s | tail], [s | tail_match], binds) do
    list_match(tail, tail_match, binds)
  end

  def list_match([segment | tail], [{binding, [{:_, _}]} | matchers], bindings) do
    put_binding(bindings, binding, segment, tail, matchers)
  end

  def list_match([segment | tail], [{binding, [segment]} | matchers], bindings)
      when is_atom(binding) do
    put_binding(bindings, binding, segment, tail, matchers)
  end

  def list_match(rest, [{binding, [{any, _}]}], bindings)
      when is_atom(binding) and any in [:_, :__] do
    value = Enum.join(rest, "/")

    list_match([], [], Map.put(bindings, binding, value))
  end

  def list_match([segment | _] = rest, [{binding, [segment, {any, _}]}], bindings)
      when is_atom(binding) and any in [:_, :__] do
    value = Enum.join(rest, "/")

    list_match([], [], Map.put(bindings, binding, value))
  end

  def list_match(
        [segment | tail],
        [{binding, [segment | sub_matches]} | matches],
        bindings
      )
      when is_atom(binding) do
    end_condition =
      case matches do
        [next | _] -> next
        [] -> :undefined
      end

    with {matched_segments, tail} <- match_until(tail, end_condition, sub_matches, []) do
      value = Enum.join([segment | matched_segments], "/")
      bindings = Map.put(bindings, binding, value)

      list_match(tail, matches, bindings)
    end
  end

  def list_match([segment | tail], [{binding, []} | matchers], bindings) when is_atom(binding) do
    put_binding(bindings, binding, segment, tail, matchers)
  end

  def list_match([], [], binds) do
    {true, binds, :undefined}
  end

  def list_match(_list, _match, _binds) do
    false
  end

  # End recursion, since there's no "outside" matches we should iterate to end of segments
  def match_until([], :undefined, [], acc) do
    {Enum.reverse(acc), []}
  end

  # End recursion, end condition is a binding with a matching complex start segment
  def match_until(
        [segment | _] = segments,
        _end_condition = {binding, [segment | _]},
        [],
        acc
      )
      when is_atom(binding) do
    {Enum.reverse(acc), segments}
  end

  # End recursion since the submatch contains a trailing wildcard but we have more matches "outside" this sub-segment
  def match_until([segment | _] = segments, _end_condition = segment, [], acc) do
    {Enum.reverse(acc), segments}
  end

  # Reached the "end" of this wildcard, so we proceed with the next match
  def match_until([_segment | _] = segments, end_condition, [{:__, []}, match | matches], acc) do
    match_until(segments, end_condition, [match | matches], acc)
  end

  # Segment is matching the wildcard and have not reached "end" of wildcard
  def match_until([segment | segments], end_condition, [{:__, []} | _] = matches, acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # Current match is matching segment, add to accumulator and set next match as the current one
  def match_until([segment | segments], end_condition, [segment | matches], acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # 'Any' match is matching first segment, add to accumulator and set next match as the current one
  def match_until([segment | segments], end_condition, [{:_, []} | matches], acc) do
    match_until(segments, end_condition, matches, [segment | acc])
  end

  # No match
  def match_until(_segments, _end_condition, _matches, _acc) do
    false
  end

  defp put_binding(bindings, binding, value, tail, matchers) do
    case bindings do
      %{^binding => ^value} ->
        list_match(tail, matchers, bindings)

      %{^binding => _} ->
        false

      _ ->
        list_match(tail, matchers, Map.put(bindings, binding, value))
    end
  end
end

defmodule Middleware do
  def wrap(mod, {func_name, 2}) do
    fn(next) ->
      fn(a,b) ->
        apply(mod, func_name, [a,b, next])
      end
    end
  end

  def wrap(mod, {func_name, 3}) do
    fn(next) ->
      fn(a,b,c) ->
        apply(mod, func_name, [a, b, c, next])
      end
    end
  end

  def wrap(mod, {func_name, 4}) do
    fn(next) ->
      fn(a,b,c,d) ->
        apply(mod, func_name, [a, b, c, d, next])
      end
    end
  end

  def wrap(mod, {func_name, 6}) do
    fn(next) ->
      fn(a,b,c,d,e,f) ->
        apply(mod, func_name, [a, b, c, d, e, f, next])
      end
    end
  end

  def build_chain(mods, final_fn, {_func_name, _arity} = fa) do
    mods
    |> Enum.map(&(wrap(&1, fa)))
    |> Enum.reduce(final_fn, fn(curr, next) ->
      curr.(next)
    end)
  end
end

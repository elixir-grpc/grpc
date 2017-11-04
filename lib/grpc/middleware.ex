defmodule Middleware do
  def wrap(mod) do
    fn(next) ->
      # TODO rewrite
      fn(a,b,c,d) ->
        mod.call(a,b,c,d,next)
      end
    end
  end

  def build_chain(mods, final_fn) do
    mods
    |> Enum.map(&wrap/1)
    |> Enum.reduce(final_fn, fn(curr, next) ->
      curr.(next)
    end)
  end
end

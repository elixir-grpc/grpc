defmodule Middleware do
  def wrap(mod) do
    fn(next) ->
      # TODO rewrite
      fn(a,b,c,d) ->
        mod.call(a,b,c,d,next)
      end
    end
  end

  def build_chain(mods, wrapped_call) do
    # TODO rewrite
    fns = Enum.map(mods, &wrap/1)
    Enum.reduce(fns, wrapped_call, fn(curr, next) ->
      curr.(next) # fn(a,b,c)
    end)
  end
end

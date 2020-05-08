defmodule GRPC.Adapter.Cowboy.HandlerError do
  defexception [:req, :kind, :reason, :stack]

  def new(req, %{__exception__: _} = e, stack \\ []) do
    exception(req: req, kind: :error, reason: e, stack: stack)
  end

  def exception(params) when is_list(params) do
    struct(__MODULE__, params)
  end

  def message(%{req: req, kind: kind, reason: reason, stack: stack}) do
    path = :cowboy_req.path(req)
    "While handling #{path}:\n  " <> Exception.format_banner(kind, reason, stack)
  end

  def reraise(%__MODULE__{} = error) do
    :erlang.raise(:error, error, error.stack)
  end
end

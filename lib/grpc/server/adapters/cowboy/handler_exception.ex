defmodule GRPC.Server.Adapters.Cowboy.HandlerException do
  defexception [:req, :kind, :reason, :stack]

  def new(req, %{__exception__: _} = exception, stack \\ [], kind \\ :error) do
    exception(req: req, kind: kind, reason: exception, stack: stack)
  end

  def message(%__MODULE__{req: req, kind: kind, reason: reason, stack: stack}) do
    path = :cowboy_req.path(req)
    "Exception raised while handling #{path}:\n" <> Exception.format_banner(kind, reason, stack)
  end
end

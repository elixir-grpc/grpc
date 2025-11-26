defmodule Mint.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    persistent_term =
      Code.ensure_loaded?(:persistent_term) and function_exported?(:persistent_term, :get, 2)

    Application.put_env(:mint, :persistent_term, persistent_term)

    opts = [strategy: :one_for_one, name: Mint.Supervisor]
    Supervisor.start_link([], opts)
  end
end

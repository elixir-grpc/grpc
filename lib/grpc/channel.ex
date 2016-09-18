defmodule GRPC.Channel do
  defstruct [:host, :port, :pid, :scheme]

  @spec connect(String.t, Integer.t, Keyword.t) :: {:ok, struct} | {:error, any}
  def connect(addr, opts) when is_binary(addr) do
    [host, port] = String.split(addr, ":")
    connect(host, port, opts)
  end
  def connect(host, port, opts) when is_binary(port) do
    connect(host, String.to_integer(port), opts)
  end
  def connect(host, port, opts) when is_integer(port) do
    if opts[:insecure] do
      connect_insecurely(host, port)
    else
      # TODO: Secure connection
    end
  end

  defp connect_insecurely(host, port) do
    case :h2_client.start_link(:http, String.to_charlist(host), port) do
      {:ok, pid} ->
        channel = %__MODULE__{host: host, port: port, pid: pid, scheme: "http"}
        {:ok, channel}
      err = {:error, _} -> err
      _ -> {:error, "Unknown error!"}
    end
  end
end

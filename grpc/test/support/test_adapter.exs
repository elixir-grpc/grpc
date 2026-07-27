defmodule GRPC.Test.ClientAdapter do
  @behaviour GRPC.Client.Adapter

  def connect(channel, _opts), do: {:ok, channel}
  def disconnect(channel), do: {:ok, channel}
  def send_request(stream, _message, _opts), do: stream
  def receive_data(_stream, _opts), do: {:ok, nil}
  def send_data(stream, _message, _opts), do: stream
  def send_headers(stream, _opts), do: stream
  def end_stream(stream), do: stream
  def cancel(stream), do: stream
end

defmodule GRPC.Test.FailingClientAdapter do
  @moduledoc """
  A test adapter that refuses to connect to selected hosts. All other hosts
  succeed.

  The failing set comes from the `:failing_hosts` adapter option: either a
  list of hosts, or a zero-arity function returning one for tests that flip
  reachability mid-test. Per-connection options keep tests free of global
  state, so they can run `async: true`.

      adapter: GRPC.Test.FailingClientAdapter,
      adapter_opts: [failing_hosts: ["127.0.0.1"]]

      hosts = start_supervised!({Agent, fn -> ["127.0.0.1"] end})
      adapter_opts: [failing_hosts: fn -> Agent.get(hosts, & &1) end]
  """
  @behaviour GRPC.Client.Adapter

  def connect(%{host: host} = channel, opts) do
    if host in failing_hosts(opts) do
      {:error, :connection_refused}
    else
      {:ok, channel}
    end
  end

  defp failing_hosts(opts) do
    case Keyword.get(opts || [], :failing_hosts, []) do
      fun when is_function(fun, 0) -> fun.()
      hosts when is_list(hosts) -> hosts
    end
  end

  def disconnect(channel), do: {:ok, channel}
  def send_request(stream, _message, _opts), do: stream
  def receive_data(_stream, _opts), do: {:ok, nil}
  def send_data(stream, _message, _opts), do: stream
  def send_headers(stream, _opts), do: stream
  def end_stream(stream), do: stream
  def cancel(stream), do: stream
end

defmodule GRPC.Test.ServerAdapter do
  @behaviour GRPC.Server.Adapter

  def start(s, h, p, opts) do
    {s, h, p, opts}
  end

  def stop(server) do
    {server}
  end

  def stop(endpoint, server) do
    {endpoint, server}
  end

  def send_reply(stream, data, _opts) do
    {stream, data}
  end

  def send_headers(stream, _headers) do
    stream
  end

  def has_sent_headers?(_stream) do
    false
  end

  def set_headers(stream, headers) do
    send(self(), {:setting_headers, headers})
    stream
  end
end

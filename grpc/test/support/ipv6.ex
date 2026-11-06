defmodule GRPC.Test.IPv6 do
  @moduledoc false

  # Helpers for tests that need IPv6.

  @loopback {0, 0, 0, 0, 0, 0, 0, 1}

  def loopback, do: @loopback

  @doc "True when this machine can listen on the IPv6 loopback."
  def loopback_available? do
    case :gen_tcp.listen(0, [:inet6, ip: @loopback]) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, _} ->
        false
    end
  end

  @doc """
  Makes the name `aaaa-only.test` resolve to `::1` and nothing else, for the
  calling test only.

  It points the Erlang resolver at a hosts file in `dir` instead of the system
  one, and puts the old settings back when the test ends. The resolver settings
  are global, so a test using this must not be `async`.
  """
  def aaaa_only_host(dir) do
    hosts_file = Path.join(dir, "hosts")

    File.write!(
      hosts_file,
      "::1 aaaa-only.test\n127.0.0.1 dual-stack.test\n::1 dual-stack.test\n"
    )

    old_lookup = :inet_db.res_option(:lookup)
    old_hosts_file = :inet_db.res_option(:hosts_file)
    :inet_db.set_lookup([:file])
    :inet_db.set_hosts_file(String.to_charlist(hosts_file))

    ExUnit.Callbacks.on_exit(fn ->
      :inet_db.set_lookup(old_lookup)
      :inet_db.set_hosts_file(old_hosts_file)
    end)

    "aaaa-only.test"
  end
end

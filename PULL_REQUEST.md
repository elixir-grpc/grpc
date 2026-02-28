## Fix two compiler warnings/errors in `grpc_client`

### Summary

This PR fixes two bugs caught by the Elixir compiler in the `grpc_client` package, and adds tests for the first fix.

---

### Fix 1 — `Connection.connect/2`: wrong argument passed to `pick_channel/2`

**File:** `lib/grpc/client/connection.ex`

#### Problem

In `Connection.connect/2`, when `DynamicSupervisor` returned `{:error, {:already_started, _pid}}` (i.e. a connection for that ref was already running), the code fell into this branch:

```elixir
{:error, {:already_started, _pid}} ->
  case pick_channel(opts) do
    {:ok, %Channel{} = channel} ->
      {:ok, channel}
    _ ->
      {:error, :no_connection}
  end
```

`pick_channel/2` is defined as:

```elixir
def pick_channel(%Channel{ref: ref} = _channel, _opts \\ []) do
  case :persistent_term.get({__MODULE__, :lb_state, ref}, nil) do
    nil -> {:error, :no_connection}
    %Channel{} = channel -> {:ok, channel}
  end
end
```

It expects a `%GRPC.Channel{}` struct as its first argument in order to extract the `ref` used to look up the channel in `:persistent_term`. Passing `opts` (a keyword list) instead would cause a `FunctionClauseError` at runtime every time a client tried to reuse an existing connection.

#### Fix

Pass `ch` (the virtual channel already built from `initial_state`) as the first argument:

```elixir
{:error, {:already_started, _pid}} ->
  case pick_channel(ch, opts) do
    {:ok, %Channel{} = channel} ->
      {:ok, channel}
    _ ->
      {:error, :no_connection}
  end
```

#### Tests

This fix ships with a new test file **`test/grpc/client/connection_test.exs`** covering:

- `pick_channel/2` in isolation — both the `:no_connection` and the happy path.
- The `already_started` branch of `connect/2` that was broken.
- Disconnect behaviour and the intentional persistence of the `:persistent_term` entry after disconnect.

---

### Fix 2 — `ConnectionProcess.chunk_body/2`: unpinned variable in bitstring size specifier

**File:** `lib/grpc/client/adapters/mint/connection_process/connection_process.ex`

#### Problem

The private `chunk_body/2` function splits a binary payload into a chunk that fits within the current HTTP/2 window size and a tail to be re-enqueued:

```elixir
defp chunk_body(body, bytes_length) do
  case body do
    <<head::binary-size(bytes_length), tail::binary>> -> {head, tail}
    _other -> {body, <<>>}
  end
end
```

As of Elixir 1.17, the compiler requires variables that are defined **outside** a pattern match and referenced inside a `size(...)` specifier to be pinned with `^`. Without it, the compiler emits a warning today and will make it an error in a future release.

#### Fix

Pin the variable with `^`:

```elixir
defp chunk_body(body, bytes_length) do
  case body do
    <<head::binary-size(^bytes_length), tail::binary>> -> {head, tail}
    _other -> {body, <<>>}
  end
end
```

#### Tests

This code path was already covered by the pre-existing test in `test/grpc/adapters/mint/connection_process_test.exs`:

> *(body_size > window_size) chunk payload stream what is possible and enqueue the rest at the beginning of the queue to give priority to the current request*

No new tests were needed.
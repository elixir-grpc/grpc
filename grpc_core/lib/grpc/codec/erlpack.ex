defmodule GRPC.Codec.Erlpack do
  @moduledoc """
  Codec that serializes messages using the Erlang external term format.

  Decoding hardens against untrusted gRPC payloads (CVE-2026-48853 /
  GHSA-grp7-v8xh-rj7h):

    * `:erlang.binary_to_term/2` is called with the `:safe` option, which
      prevents the payload from creating new atoms (atom-table exhaustion DoS).
    * The decoded term is then rejected if it contains a function, pid, port or
      reference. `:safe` alone does not block fun materialization on every OTP
      release, and a materialized fun reaching a call site enables remote code
      execution. None of these types are valid in a gRPC payload.

  As a consequence, any atom referenced by an incoming payload must already
  exist in the receiving node, which is the case for loaded protobuf structs.
  """

  @behaviour GRPC.Codec

  def name() do
    "erlpack"
  end

  def encode(struct, _opts \\ []) do
    :erlang.term_to_binary(struct)
  end

  def decode(binary, _module) do
    term = :erlang.binary_to_term(binary, [:safe])
    ensure_safe_term!(term)
    term
  end

  defp ensure_safe_term!(term)
       when is_function(term) or is_pid(term) or is_port(term) or is_reference(term) do
    raise ArgumentError,
          "refusing to decode unsafe erlpack payload containing a #{term_type(term)}"
  end

  defp ensure_safe_term!(term) when is_list(term) do
    Enum.each(term, &ensure_safe_term!/1)
  end

  defp ensure_safe_term!(term) when is_tuple(term) do
    term |> Tuple.to_list() |> Enum.each(&ensure_safe_term!/1)
  end

  defp ensure_safe_term!(term) when is_map(term) do
    # `Map.to_list/1` works for plain maps and structs alike, unlike `Enum`,
    # which is not implemented for structs.
    Enum.each(Map.to_list(term), fn {key, value} ->
      ensure_safe_term!(key)
      ensure_safe_term!(value)
    end)
  end

  defp ensure_safe_term!(_term), do: :ok

  defp term_type(term) when is_function(term), do: "function"
  defp term_type(term) when is_pid(term), do: "pid"
  defp term_type(term) when is_port(term), do: "port"
  defp term_type(term) when is_reference(term), do: "reference"
end

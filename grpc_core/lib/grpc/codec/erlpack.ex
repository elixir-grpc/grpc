defmodule GRPC.Codec.Erlpack do
  @moduledoc """
  Codec that serializes messages using the Erlang external term format.

  Decoding uses `:erlang.binary_to_term/2` with the `:safe` option so that
  untrusted gRPC payloads cannot create new atoms or materialize fun terms.
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
    # `:safe` prevents decoding of untrusted payloads into new atoms or
    # executable fun terms, which would otherwise allow atom-table exhaustion
    # DoS and remote code execution (CVE-2026-48853 / GHSA-grp7-v8xh-rj7h).
    :erlang.binary_to_term(binary, [:safe])
  end
end

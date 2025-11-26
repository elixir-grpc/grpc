defmodule HPAX do
  @moduledoc """
  Support for the HPACK header compression algorithm.

  This module provides support for the HPACK header compression algorithm used mainly in HTTP/2.

  ## Encoding and decoding contexts

  The HPACK algorithm requires both

    * an encoding context on the encoder side
    * a decoding context on the decoder side

  These contexts are semantically different but structurally the same. In HPACK they are
  implemented as **HPACK tables**. This library uses the name "tables" everywhere internally

  HPACK tables can be created through the `new/1` function.
  """

  alias HPAX.{Table, Types}

  @typedoc """
  An HPACK table.

  This can be used for encoding or decoding.
  """
  @typedoc since: "0.2.0"
  @opaque table() :: Table.t()

  @typedoc """
  An HPACK header name.
  """
  @type header_name() :: binary()

  @typedoc """
  An HPACK header value.
  """
  @type header_value() :: binary()

  @valid_header_actions [:store, :store_name, :no_store, :never_store]

  @doc """
  Creates a new HPACK table.

  Same as `new/2` with default options.
  """
  @spec new(non_neg_integer()) :: table()
  def new(max_table_size), do: new(max_table_size, [])

  @doc """
  Create a new HPACK table that can be used as encoding or decoding context.

  See the "Encoding and decoding contexts" section in the module documentation.

  `max_table_size` is the maximum table size (in bytes) for the newly created table.

  ## Options

  This function accepts the following `options`:

    * `:huffman_encoding` - (since 0.2.0) `:always` or `:never`. If `:always`,
      then HPAX will always encode headers using Huffman encoding. If `:never`,
      HPAX will not use any Huffman encoding. Defaults to `:never`.

  ## Examples

      encoding_context = HPAX.new(4096)

  """
  @doc since: "0.2.0"
  @spec new(non_neg_integer(), [keyword()]) :: table()
  def new(max_table_size, options)
      when is_integer(max_table_size) and max_table_size >= 0 and is_list(options) do
    options = Keyword.put_new(options, :huffman_encoding, :never)

    Enum.each(options, fn
      {:huffman_encoding, _huffman_encoding} -> :ok
      {key, _value} -> raise ArgumentError, "unknown option: #{inspect(key)}"
    end)

    Table.new(max_table_size, Keyword.fetch!(options, :huffman_encoding))
  end

  @doc """
  Resizes the given table to the given maximum size.

  This is intended for use where the overlying protocol has signaled a change to the table's
  maximum size, such as when an HTTP/2 `SETTINGS` frame is received.

  If the indicated size is less than the table's current size, entries
  will be evicted as needed to fit within the specified size, and the table's
  maximum size will be decreased to the specified value. A flag will also be
  set which will enqueue a "dynamic table size update" command to be prefixed
  to the next block encoded with this table, per
  [RFC9113§4.3.1](https://www.rfc-editor.org/rfc/rfc9113.html#section-4.3.1).

  If the indicated size is greater than or equal to the table's current max size, no entries are evicted
  and the table's maximum size changes to the specified value.

  ## Examples

      decoding_context = HPAX.new(4096)
      HPAX.resize(decoding_context, 8192)

  """
  @spec resize(table(), non_neg_integer()) :: table()
  defdelegate resize(table, new_max_size), to: Table

  @doc """
  Decodes a header block fragment (HBF) through a given table.

  If decoding is successful, this function returns a `{:ok, headers, updated_table}` tuple where
  `headers` is a list of decoded headers, and `updated_table` is the updated table. If there's
  an error in decoding, this function returns `{:error, reason}`.

  ## Examples

      decoding_context = HPAX.new(1000)
      hbf = get_hbf_from_somewhere()
      HPAX.decode(hbf, decoding_context)
      #=> {:ok, [{":method", "GET"}], decoding_context}

  """
  @spec decode(binary(), table()) ::
          {:ok, [{header_name(), header_value()}], table()} | {:error, term()}

  # Dynamic resizes must occur only at the start of a block
  # https://datatracker.ietf.org/doc/html/rfc7541#section-4.2
  def decode(<<0b001::3, rest::bitstring>>, %Table{} = table) do
    {new_max_size, rest} = decode_integer(rest, 5)

    # Dynamic resizes must be less than protocol max table size
    # https://datatracker.ietf.org/doc/html/rfc7541#section-6.3
    if new_max_size <= table.protocol_max_table_size do
      decode(rest, Table.dynamic_resize(table, new_max_size))
    else
      {:error, :protocol_error}
    end
  end

  def decode(block, %Table{} = table) when is_binary(block) do
    decode_headers(block, table, _acc = [])
  catch
    :throw, {:hpax, error} -> {:error, error}
  end

  @doc """
  Encodes a list of headers through the given table.

  Returns a two-element tuple where the first element is a binary representing the encoded headers
  and the second element is an updated table.

  ## Examples

      headers = [{:store, ":authority", "https://example.com"}]
      encoding_context = HPAX.new(1000)
      HPAX.encode(headers, encoding_context)
      #=> {iodata, updated_encoding_context}

  """
  @spec encode([header], table()) :: {iodata(), table()}
        when header: {action, header_name(), header_value()},
             action: :store | :store_name | :no_store | :never_store
  def encode(headers, %Table{} = table) when is_list(headers) do
    {table, pending_resizes} = Table.pop_pending_resizes(table)
    acc = Enum.map(pending_resizes, &[<<0b001::3, Types.encode_integer(&1, 5)::bitstring>>])
    encode_headers(headers, table, acc)
  end

  @doc """
  Encodes a list of headers through the given table, applying the same `action` to all of them.

  This function is the similar to `encode/2`, but `headers` are `{name, value}` tuples instead,
  and the same `action` is applied to all headers.

    ## Examples

      headers = [{":authority", "https://example.com"}]
      encoding_context = HPAX.new(1000)
      HPAX.encode(:store, headers, encoding_context)
      #=> {iodata, updated_encoding_context}

  """
  @doc since: "0.2.0"
  @spec encode(action, [header], table()) :: {iodata(), table()}
        when action: :store | :store_name | :no_store | :never_store,
             header: {header_name(), header_value()}
  def encode(action, headers, %Table{} = table)
      when is_list(headers) and action in [:store, :store_name, :no_store, :never_store] do
    headers
    |> Enum.map(fn {name, value} -> {action, name, value} end)
    |> encode(table)
  end

  ## Helpers

  defp decode_headers(<<>>, table, acc) do
    {:ok, Enum.reverse(acc), table}
  end

  # Indexed header field
  # http://httpwg.org/specs/rfc7541.html#rfc.section.6.1
  defp decode_headers(<<0b1::1, rest::bitstring>>, table, acc) do
    {index, rest} = decode_integer(rest, 7)
    decode_headers(rest, table, [lookup_by_index!(table, index) | acc])
  end

  # Literal header field with incremental indexing
  # http://httpwg.org/specs/rfc7541.html#rfc.section.6.2.1
  defp decode_headers(<<0b01::2, rest::bitstring>>, table, acc) do
    {name, value, rest} =
      case rest do
        # The header name is a string.
        <<0::6, rest::binary>> ->
          {name, rest} = decode_binary(rest)
          {value, rest} = decode_binary(rest)
          {name, value, rest}

        # The header name is an index to be looked up in the table.
        _other ->
          {index, rest} = decode_integer(rest, 6)
          {value, rest} = decode_binary(rest)
          {name, _value} = lookup_by_index!(table, index)
          {name, value, rest}
      end

    decode_headers(rest, Table.add(table, name, value), [{name, value} | acc])
  end

  # Literal header field without indexing
  # http://httpwg.org/specs/rfc7541.html#rfc.section.6.2.2
  defp decode_headers(<<0b0000::4, rest::bitstring>>, table, acc) do
    {name, value, rest} =
      case rest do
        <<0::4, rest::binary>> ->
          {name, rest} = decode_binary(rest)
          {value, rest} = decode_binary(rest)
          {name, value, rest}

        _other ->
          {index, rest} = decode_integer(rest, 4)
          {value, rest} = decode_binary(rest)
          {name, _value} = lookup_by_index!(table, index)
          {name, value, rest}
      end

    decode_headers(rest, table, [{name, value} | acc])
  end

  # Literal header field never indexed
  # http://httpwg.org/specs/rfc7541.html#rfc.section.6.2.3
  defp decode_headers(<<0b0001::4, rest::bitstring>>, table, acc) do
    {name, value, rest} =
      case rest do
        <<0::4, rest::binary>> ->
          {name, rest} = decode_binary(rest)
          {value, rest} = decode_binary(rest)
          {name, value, rest}

        _other ->
          {index, rest} = decode_integer(rest, 4)
          {value, rest} = decode_binary(rest)
          {name, _value} = lookup_by_index!(table, index)
          {name, value, rest}
      end

    # TODO: enforce the "never indexed" part somehow.
    decode_headers(rest, table, [{name, value} | acc])
  end

  defp decode_headers(_other, _table, _acc) do
    throw({:hpax, :protocol_error})
  end

  defp lookup_by_index!(table, index) do
    case Table.lookup_by_index(table, index) do
      {:ok, header} -> header
      :error -> throw({:hpax, {:index_not_found, index}})
    end
  end

  defp decode_integer(bitstring, prefix) do
    case Types.decode_integer(bitstring, prefix) do
      {:ok, int, rest} -> {int, rest}
      :error -> throw({:hpax, :bad_integer_encoding})
    end
  end

  defp decode_binary(binary) do
    case Types.decode_binary(binary) do
      {:ok, binary, rest} -> {binary, rest}
      :error -> throw({:hpax, :bad_binary_encoding})
    end
  end

  defp encode_headers([], table, acc) do
    {acc, table}
  end

  defp encode_headers([{action, name, value} | rest], table, acc)
       when action in @valid_header_actions and is_binary(name) and is_binary(value) do
    huffman? = table.huffman_encoding == :always

    {encoded, table} =
      case Table.lookup_by_header(table, name, value) do
        {:full, index} ->
          {encode_indexed_header(index), table}

        {:name, index} when action == :store ->
          {encode_literal_header_with_indexing(index, value, huffman?),
           Table.add(table, name, value)}

        {:name, index} when action in [:store_name, :no_store] ->
          {encode_literal_header_without_indexing(index, value, huffman?), table}

        {:name, index} when action == :never_store ->
          {encode_literal_header_never_indexed(index, value, huffman?), table}

        :not_found when action in [:store, :store_name] ->
          {encode_literal_header_with_indexing(name, value, huffman?),
           Table.add(table, name, value)}

        :not_found when action == :no_store ->
          {encode_literal_header_without_indexing(name, value, huffman?), table}

        :not_found when action == :never_store ->
          {encode_literal_header_never_indexed(name, value, huffman?), table}
      end

    encode_headers(rest, table, [acc, encoded])
  end

  defp encode_indexed_header(index) do
    <<1::1, Types.encode_integer(index, 7)::bitstring>>
  end

  defp encode_literal_header_with_indexing(index, value, huffman?) when is_integer(index) do
    [<<1::2, Types.encode_integer(index, 6)::bitstring>>, Types.encode_binary(value, huffman?)]
  end

  defp encode_literal_header_with_indexing(name, value, huffman?) when is_binary(name) do
    [<<1::2, 0::6>>, Types.encode_binary(name, huffman?), Types.encode_binary(value, huffman?)]
  end

  defp encode_literal_header_without_indexing(index, value, huffman?) when is_integer(index) do
    [<<0::4, Types.encode_integer(index, 4)::bitstring>>, Types.encode_binary(value, huffman?)]
  end

  defp encode_literal_header_without_indexing(name, value, huffman?) when is_binary(name) do
    [<<0::4, 0::4>>, Types.encode_binary(name, huffman?), Types.encode_binary(value, huffman?)]
  end

  defp encode_literal_header_never_indexed(index, value, huffman?) when is_integer(index) do
    [<<1::4, Types.encode_integer(index, 4)::bitstring>>, Types.encode_binary(value, huffman?)]
  end

  defp encode_literal_header_never_indexed(name, value, huffman?) when is_binary(name) do
    [<<1::4, 0::4>>, Types.encode_binary(name, huffman?), Types.encode_binary(value, huffman?)]
  end
end

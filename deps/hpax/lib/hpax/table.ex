defmodule HPAX.Table do
  @moduledoc false

  @enforce_keys [:max_table_size, :huffman_encoding]
  defstruct [
    :protocol_max_table_size,
    :max_table_size,
    :huffman_encoding,
    entries: [],
    size: 0,
    length: 0,
    pending_minimum_resize: nil
  ]

  @type huffman_encoding() :: :always | :never

  @type t() :: %__MODULE__{
          protocol_max_table_size: non_neg_integer(),
          max_table_size: non_neg_integer(),
          huffman_encoding: huffman_encoding(),
          entries: [{binary(), binary()}],
          size: non_neg_integer(),
          length: non_neg_integer(),
          pending_minimum_resize: non_neg_integer() | nil
        }

  @static_table [
    {":authority", nil},
    {":method", "GET"},
    {":method", "POST"},
    {":path", "/"},
    {":path", "/index.html"},
    {":scheme", "http"},
    {":scheme", "https"},
    {":status", "200"},
    {":status", "204"},
    {":status", "206"},
    {":status", "304"},
    {":status", "400"},
    {":status", "404"},
    {":status", "500"},
    {"accept-charset", nil},
    {"accept-encoding", "gzip, deflate"},
    {"accept-language", nil},
    {"accept-ranges", nil},
    {"accept", nil},
    {"access-control-allow-origin", nil},
    {"age", nil},
    {"allow", nil},
    {"authorization", nil},
    {"cache-control", nil},
    {"content-disposition", nil},
    {"content-encoding", nil},
    {"content-language", nil},
    {"content-length", nil},
    {"content-location", nil},
    {"content-range", nil},
    {"content-type", nil},
    {"cookie", nil},
    {"date", nil},
    {"etag", nil},
    {"expect", nil},
    {"expires", nil},
    {"from", nil},
    {"host", nil},
    {"if-match", nil},
    {"if-modified-since", nil},
    {"if-none-match", nil},
    {"if-range", nil},
    {"if-unmodified-since", nil},
    {"last-modified", nil},
    {"link", nil},
    {"location", nil},
    {"max-forwards", nil},
    {"proxy-authenticate", nil},
    {"proxy-authorization", nil},
    {"range", nil},
    {"referer", nil},
    {"refresh", nil},
    {"retry-after", nil},
    {"server", nil},
    {"set-cookie", nil},
    {"strict-transport-security", nil},
    {"transfer-encoding", nil},
    {"user-agent", nil},
    {"vary", nil},
    {"via", nil},
    {"www-authenticate", nil}
  ]

  @static_table_size length(@static_table)
  @dynamic_table_start @static_table_size + 1

  @doc """
  Creates a new HPACK table with the given maximum size.

  The maximum size is not the maximum number of entries but rather the maximum size as defined in
  http://httpwg.org/specs/rfc7541.html#maximum.table.size.
  """
  @spec new(non_neg_integer(), huffman_encoding()) :: t()
  def new(protocol_max_table_size, huffman_encoding)
      when is_integer(protocol_max_table_size) and protocol_max_table_size >= 0 and
             huffman_encoding in [:always, :never] do
    %__MODULE__{
      protocol_max_table_size: protocol_max_table_size,
      max_table_size: protocol_max_table_size,
      huffman_encoding: huffman_encoding
    }
  end

  @doc """
  Adds the given header to the given table.

  If the new entry does not fit within the max table size then the oldest entries will be evicted.

  Header names should be lowercase when added to the HPACK table
  as per the [HTTP/2 spec](https://http2.github.io/http2-spec/#rfc.section.8.1.2):

  > header field names MUST be converted to lowercase prior to their encoding in HTTP/2

  """
  @spec add(t(), binary(), binary()) :: t()
  def add(%__MODULE__{} = table, name, value) do
    %{max_table_size: max_table_size, size: size} = table
    entry_size = entry_size(name, value)

    cond do
      # An attempt to add an entry larger than the maximum size causes the table to be emptied of
      # all existing entries and results in an empty table.
      entry_size > max_table_size ->
        %{table | entries: [], size: 0, length: 0}

      size + entry_size > max_table_size ->
        table
        |> evict_to_size(max_table_size - entry_size)
        |> add_header(name, value, entry_size)

      true ->
        add_header(table, name, value, entry_size)
    end
  end

  defp add_header(%__MODULE__{} = table, name, value, entry_size) do
    %{entries: entries, size: size, length: length} = table
    %{table | entries: [{name, value} | entries], size: size + entry_size, length: length + 1}
  end

  @doc """
  Looks up a header by index `index` in the given `table`.

  Returns `{:ok, {name, value}}` if a header is found at the given `index`, otherwise returns
  `:error`. `value` can be a binary in case both the header name and value are present in the
  table, or `nil` if only the name is present (this can only happen in the static table).
  """
  @spec lookup_by_index(t(), pos_integer()) :: {:ok, {binary(), binary() | nil}} | :error
  def lookup_by_index(table, index)

  # Static table
  for {header, index} <- Enum.with_index(@static_table, 1) do
    def lookup_by_index(%__MODULE__{}, unquote(index)), do: {:ok, unquote(header)}
  end

  def lookup_by_index(%__MODULE__{length: 0}, _index) do
    :error
  end

  def lookup_by_index(%__MODULE__{entries: entries, length: length}, index)
      when index >= @dynamic_table_start and index <= @dynamic_table_start + length - 1 do
    {:ok, Enum.at(entries, index - @dynamic_table_start)}
  end

  def lookup_by_index(%__MODULE__{}, _index) do
    :error
  end

  @doc """
  Looks up the index of a header by its name and value.

  It returns:

    * `{:full, index}` if the full header (name and value) are present in the table at `index`

    * `{:name, index}` if `name` is present in the table but with a different value than `value`

    * `:not_found` if the header name is not in the table at all

  Header names should be lowercase when looked up in the HPACK table
  as per the [HTTP/2 spec](https://http2.github.io/http2-spec/#rfc.section.8.1.2):

  > header field names MUST be converted to lowercase prior to their encoding in HTTP/2

  """
  @spec lookup_by_header(t(), binary(), binary() | nil) ::
          {:full, pos_integer()} | {:name, pos_integer()} | :not_found
  def lookup_by_header(table, name, value)

  def lookup_by_header(%__MODULE__{entries: entries}, name, value) do
    case static_lookup_by_header(name, value) do
      {:full, _index} = result ->
        result

      {:name, index} ->
        # Check if we get full match in the dynamic tabble
        case dynamic_lookup_by_header(entries, name, value, @dynamic_table_start, nil) do
          {:full, _index} = result -> result
          _other -> {:name, index}
        end

      :not_found ->
        dynamic_lookup_by_header(entries, name, value, @dynamic_table_start, nil)
    end
  end

  for {{name, value}, index} when is_binary(value) <- Enum.with_index(@static_table, 1) do
    defp static_lookup_by_header(unquote(name), unquote(value)) do
      {:full, unquote(index)}
    end
  end

  static_table_names =
    @static_table
    |> Enum.map(&elem(&1, 0))
    |> Enum.with_index(1)
    |> Enum.uniq_by(&elem(&1, 0))

  for {name, index} <- static_table_names do
    defp static_lookup_by_header(unquote(name), _value) do
      {:name, unquote(index)}
    end
  end

  defp static_lookup_by_header(_name, _value) do
    :not_found
  end

  defp dynamic_lookup_by_header([{name, value} | _rest], name, value, index, _name_index) do
    {:full, index}
  end

  defp dynamic_lookup_by_header([{name, _} | rest], name, value, index, _name_index) do
    dynamic_lookup_by_header(rest, name, value, index + 1, index)
  end

  defp dynamic_lookup_by_header([_other | rest], name, value, index, name_index) do
    dynamic_lookup_by_header(rest, name, value, index + 1, name_index)
  end

  defp dynamic_lookup_by_header([], _name, _value, _index, name_index) do
    if name_index, do: {:name, name_index}, else: :not_found
  end

  @doc """
  Changes the table's protocol negotiated maximum size, possibly evicting entries as needed to satisfy.

  If the indicated size is less than the table's current max size, entries
  will be evicted as needed to fit within the specified size, and the table's
  maximum size will be decreased to the specified value. An will also be
  set which will enqueue a 'dynamic table size update' command to be prefixed
  to the next block encoded with this table, per RFC9113§4.3.1.

  If the indicated size is greater than or equal to the table's current max size, no entries are evicted
  and the table's maximum size changes to the specified value.

  In all cases, the table's `:protocol_max_table_size` is updated accordingly
  """
  @spec resize(t(), non_neg_integer()) :: t()
  def resize(%__MODULE__{} = table, new_protocol_max_table_size) do
    pending_minimum_resize =
      case table.pending_minimum_resize do
        nil -> new_protocol_max_table_size
        current -> min(current, new_protocol_max_table_size)
      end

    %{
      evict_to_size(table, new_protocol_max_table_size)
      | protocol_max_table_size: new_protocol_max_table_size,
        max_table_size: new_protocol_max_table_size,
        pending_minimum_resize: pending_minimum_resize
    }
  end

  def dynamic_resize(%__MODULE__{} = table, new_max_table_size) do
    %{
      evict_to_size(table, new_max_table_size)
      | max_table_size: new_max_table_size
    }
  end

  @doc """
  Returns (and clears) any pending resize events on the table which will need to be signalled to
  the decoder via dynamic table size update messages. Intended to be called at the start of any
  block encode to prepend such dynamic table size update(s) as needed. The value of
  `pending_minimum_resize` indicates the smallest maximum size of this table which has not yet
  been signalled to the decoder, and is always included in the list returned if it is set.
  Additionally, if the current max table size is larger than this value, it is also included int
  the list, per https://www.rfc-editor.org/rfc/rfc7541#section-4.2
  """
  def pop_pending_resizes(%__MODULE__{pending_minimum_resize: nil} = table), do: {table, []}

  def pop_pending_resizes(%__MODULE__{} = table) do
    pending_resizes =
      if table.max_table_size > table.pending_minimum_resize,
        do: [table.pending_minimum_resize, table.max_table_size],
        else: [table.pending_minimum_resize]

    {%{table | pending_minimum_resize: nil}, pending_resizes}
  end

  # Removes records as necessary to have the total size of entries within the table be less than
  # or equal to the specified value. Does not change the table's max size.
  defp evict_to_size(%__MODULE__{size: size} = table, new_size) when size <= new_size, do: table

  defp evict_to_size(%__MODULE__{entries: entries, size: size} = table, new_size) do
    {new_entries_reversed, new_size} =
      evict_towards_size(Enum.reverse(entries), size, new_size)

    %{
      table
      | entries: Enum.reverse(new_entries_reversed),
        size: new_size,
        length: length(new_entries_reversed)
    }
  end

  defp evict_towards_size([{name, value} | rest], size, max_target_size) do
    new_size = size - entry_size(name, value)

    if new_size <= max_target_size do
      {rest, new_size}
    else
      evict_towards_size(rest, new_size, max_target_size)
    end
  end

  defp evict_towards_size([], 0, _max_target_size) do
    {[], 0}
  end

  defp entry_size(name, value) do
    byte_size(name) + byte_size(value) + 32
  end

  # Made public to be used in tests.
  @doc false
  def __static_table__() do
    @static_table
  end
end

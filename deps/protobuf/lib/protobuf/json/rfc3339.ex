defmodule Protobuf.JSON.RFC3339 do
  @moduledoc false

  # The JSON mapping with Google.Protobuf.Timestamp is tricky. They use RFC3339. Elixir's built-in
  # datetime support uses ISO8601. The two are very similar but with a few small differences,
  # namely:
  # 1. RFC3339 supports nanoseconds, while the Elixir implementation goes up to microseconds.
  # 2. RFC3339 seems to be slightly stricter

  # To avoid implementing calendar awareness in here, we use a little trick: we do actually
  # manually *parse* datetimes and validate that they are correct and we also read nanoseconds out
  # of them. Then, we use the ISO8601-based Elixir functions to actually parse the datetimes with
  # calendar awareness, we throw away anything after the seconds, and replace them with the
  # nanoseconds we parsed. It seems to work very well, as proved by the conformance tests!
  # For encoding, we have to use a "dirtier" trick. We encode using Elixir's ISO8601 without
  # anything after the seconds, then we split the string and shove the nanoseconds in it. It works
  # because the DD-MM-YYYYTHH:MM:SS part has always the same size so we always know where to
  # inject the nanoseconds. Again, a bit dirty? Yes. Does it pass conformance tests? Yes!

  # The grammar for RFC3339 dates is taken straight out of the RFC
  # (https://datatracker.ietf.org/doc/html/rfc3339#section-5.6) and is reported here for ease of
  # reference:

  # date-fullyear   = 4DIGIT
  # date-month      = 2DIGIT  ; 01-12
  # date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
  #                           ; month/year
  # time-hour       = 2DIGIT  ; 00-23
  # time-minute     = 2DIGIT  ; 00-59
  # time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
  #                           ; rules
  # time-secfrac    = "." 1*DIGIT
  # time-numoffset  = ("+" / "-") time-hour ":" time-minute
  # time-offset     = "Z" / time-numoffset
  #
  # partial-time    = time-hour ":" time-minute ":" time-second
  #                   [time-secfrac]
  # full-date       = date-fullyear "-" date-month "-" date-mday
  # full-time       = partial-time time-offset
  #
  # date-time       = full-date "T" full-time
  #
  # date-time       = full-date "T" full-time

  # Most of the functions in this module are called "eat_*": these functions consume bytes but
  # don't return them. They are just used for "skipping ahead". The "parse_*" functions are the
  # only ones that return data.

  alias Protobuf.JSON.Utils

  @spec decode(String.t()) ::
          {:ok, seconds :: integer(), nanos :: non_neg_integer()} | {:error, String.t()}
  def decode(string) when is_binary(string) do
    rest = eat_full_date(string)
    rest = eat_literal(rest, "T")
    {time_secfrac_nano, rest} = parse_full_time(rest)
    ensure_empty(rest)

    case DateTime.from_iso8601(string) do
      {:ok, datetime, _offset_in_seconds} ->
        if datetime_in_allowed_range?(datetime) do
          seconds =
            datetime
            |> DateTime.truncate(:second)
            |> DateTime.to_unix(:second)

          {:ok, seconds, time_secfrac_nano}
        else
          {:error, "timestamp is outside of allowed range"}
        end

      {:error, reason} ->
        {:error, Atom.to_string(reason)}
    end
  catch
    :throw, reason -> {:error, reason}
  end

  @spec encode(integer(), non_neg_integer()) :: {:ok, String.t()} | {:error, String.t()}
  def encode(seconds, nanos)

  def encode(seconds, nanos)
      when is_integer(seconds) and is_integer(nanos) and nanos >= 1_000_000_000 do
    {:error, "nanos can't be bigger than 1000000000, got: #{nanos}"}
  end

  def encode(seconds, nanos) when is_integer(seconds) and is_integer(nanos) and nanos >= 0 do
    case DateTime.from_unix(seconds, :second) do
      {:ok, datetime} ->
        if datetime_in_allowed_range?(datetime) do
          string = DateTime.to_iso8601(datetime)

          if nanos > 0 do
            bytes_before_time_secfrac = unquote(byte_size("1970-01-01T00:00:00"))
            {before_secfrac, after_secfrac} = String.split_at(string, bytes_before_time_secfrac)
            {:ok, before_secfrac <> "." <> Utils.format_nanoseconds(nanos) <> after_secfrac}
          else
            {:ok, string}
          end
        else
          {:error, "timestamp is outside of allowed range"}
        end

      {:error, reason} ->
        {:error, inspect(reason)}
    end
  end

  ## Parsing functions

  # Grammar:
  # full-date       = date-fullyear "-" date-month "-" date-mday
  defp eat_full_date(rest) do
    rest
    |> eat_digits(_fullyear = 4)
    |> eat_literal("-")
    |> eat_digits(_month = 2)
    |> eat_literal("-")
    |> eat_digits(_mday = 2)
  end

  # Grammar:
  # full-time       = partial-time time-offset
  defp parse_full_time(string) do
    rest = eat_partial_time(string)
    {time_secfrac, rest} = parse_time_secfrac(rest)
    rest = eat_time_offset(rest)
    {time_secfrac, rest}
  end

  # Grammar:
  # partial-time    = time-hour ":" time-minute ":" time-second
  defp eat_partial_time(rest) do
    rest
    |> eat_digits(2)
    |> eat_literal(":")
    |> eat_digits(2)
    |> eat_literal(":")
    |> eat_digits(2)
  end

  # Grammar:
  # time-secfrac    = "." 1*DIGIT
  defp parse_time_secfrac("." <> rest) do
    case Utils.parse_nanoseconds(rest) do
      {secfrac_nano, rest} -> {secfrac_nano, rest}
      :error -> throw("bad time secfrac after \".\", got: #{inspect(rest)}")
    end
  end

  defp parse_time_secfrac(rest), do: {0, rest}

  defp eat_literal(string, literal) do
    literal_size = byte_size(literal)

    case string do
      <<^literal::bytes-size(literal_size), rest::binary>> -> rest
      other -> throw("expected literal #{inspect(literal)}, got: #{inspect(other)}")
    end
  end

  # Grammar:
  # time-numoffset  = ("+" / "-") time-hour ":" time-minute
  defp eat_time_offset(<<z, rest::binary>>) when z in [?z, ?Z], do: rest

  defp eat_time_offset(<<sign, rest::binary>>) when sign in [?+, ?-] do
    rest
    |> eat_digits(2)
    |> eat_literal(":")
    |> eat_digits(2)
  end

  defp eat_time_offset("") do
    throw("expected time offset, but it's missing")
  end

  defp ensure_empty(""), do: :ok
  defp ensure_empty(other), do: throw("expected empty string, got: #{inspect(other)}")

  defp eat_digits(string, count) do
    case string do
      <<digits::bytes-size(count), rest::binary>> ->
        case Integer.parse(digits) do
          {_digits, ""} ->
            rest

          _other ->
            throw("expected #{count} digits but got unparsable integer: #{inspect(digits)}")
        end

      _other ->
        throw("expected #{count} digits, got: #{inspect(string)}")
    end
  end

  {:ok, min_datetime, 0} = DateTime.from_iso8601("0001-01-01T00:00:00Z")
  {:ok, max_datetime, 0} = DateTime.from_iso8601("9999-12-31T23:59:59Z")

  defp datetime_in_allowed_range?(datetime) do
    truncated = DateTime.truncate(datetime, :second)

    DateTime.compare(truncated, unquote(Macro.escape(min_datetime))) in [:gt, :eq] and
      DateTime.compare(truncated, unquote(Macro.escape(max_datetime))) in [:lt, :eq]
  end
end

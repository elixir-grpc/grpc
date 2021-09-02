defmodule GRPC.Transport.Utils do
  @moduledoc false

  # unit: ns
  @ns_ceiling 1000
  @us_ceiling 1000_000
  # @ms_ceiling @us_ceiling * 1000

  # unit: ms
  @ms_ceiling 1000
  @second_ceiling @ms_ceiling * 60
  @minute_ceiling @second_ceiling * 60

  @doc """
  Encode timeout(nanoseconds) by gRPC protocol
  """
  def encode_ns(timeout) when timeout <= 0, do: "0u"

  def encode_ns(timeout) when is_integer(timeout) and timeout < @ns_ceiling do
    to_string(timeout) <> "n"
  end

  def encode_ns(timeout) when timeout < @us_ceiling do
    to_string(div(timeout, 1000)) <> "u"
  end

  @doc """
  Encode timeout(milliseconds) by gRPC protocol
  """
  def encode_timeout(timeout) when timeout <= 0, do: "0u"

  def encode_timeout(timeout) when is_integer(timeout) and timeout < @ms_ceiling do
    to_string(timeout) <> "m"
  end

  def encode_timeout(timeout) when timeout < @second_ceiling do
    to_string(div(timeout, 1000)) <> "S"
  end

  def encode_timeout(timeout) when timeout < @minute_ceiling do
    to_string(div(timeout, 1000 * 60)) <> "M"
  end

  def encode_timeout(timeout) do
    to_string(div(timeout, 1000 * 3600)) <> "H"
  end

  @spec decode_timeout(String.t()) :: non_neg_integer()
  def decode_timeout(timeout) do
    {timeout, unit} = String.split_at(timeout, -1)
    decode_timeout(unit, String.to_integer(timeout))
  end

  defp decode_timeout("n", timeout) do
    div(timeout, 1000_000)
  end

  defp decode_timeout("u", timeout) do
    div(timeout, 1000)
  end

  defp decode_timeout("m", timeout) do
    timeout
  end

  defp decode_timeout("S", timeout) do
    timeout * 1000
  end

  defp decode_timeout("M", timeout) do
    timeout * 60_000
  end

  defp decode_timeout("H", timeout) do
    timeout * 3_600_000
  end

  @doc """
  Encode Google.Rpc.Status message with rich error details.
  """
  @spec encode_status_details(integer, list()) :: String.t()
  def encode_status_details(_errorcode, nil), do: ""

  def encode_status_details(errorcode, details) do
    Google.Rpc.Status.new(code: errorcode, details: Enum.map(details, &build_any/1))
    |> Google.Rpc.Status.encode()
  end

  @doc """
  Decode Google.Rpc.Status message with rich error details.
  """
  @spec decode_status_details(String.t()) :: Google.Rpc.Status.t()
  def decode_status_details(details)
      when is_binary(details) do
    %Google.Rpc.Status{code: _code, message: _message, details: details} =
      Google.Rpc.Status.decode(details)

    Enum.map(details, &decode_any/1)
  end

  defp build_any(message = %{__struct__: type}) do
    Google.Protobuf.Any.new(type_url: get_type_url(type), value: Protobuf.encode(message))
  end

  def get_type_url(type) do
    [_ | type] = type |> to_string |> String.split(".")

    {type_name, package} = type |> List.pop_at(-1)

    package_name = package |> Enum.map(&String.downcase(&1)) |> Enum.join(".")

    "type.googleapis.com/#{package_name}.#{type_name}"
  end

  defp decode_any(%Google.Protobuf.Any{type_url: type_url, value: value}) do
    [_, type] = String.split(type_url, "/")
    msg_module = string_to_module(type)
    msg_module.decode(value)
  end

  defp string_to_module(type) do
    module =
      type
      |> String.split(".")
      |> Enum.map(&Macro.camelize/1)
      |> (&Enum.concat(["Elixir"], &1)).()
      |> Enum.join(".")
      |> String.to_atom()

    case Code.ensure_loaded(module) do
      {:module, module} -> module
      {:error, reason} -> raise "Failed to load module. Reason: #{inspect(reason)}"
    end
  end
end

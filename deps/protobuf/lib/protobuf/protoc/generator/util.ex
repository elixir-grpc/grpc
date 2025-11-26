defmodule Protobuf.Protoc.Generator.Util do
  @moduledoc false

  alias Protobuf.Protoc.Context

  @locals_without_parens [field: 2, field: 3, oneof: 2, rpc: 3, extend: 4, extensions: 1]

  defguardp is_nil_or_nonempty_string(term) when is_nil(term) or (is_binary(term) and term != "")

  @spec mod_name(Context.t(), [String.t()]) :: String.t()
  def mod_name(%Context{} = ctx, ns) when is_list(ns) do
    ns = Enum.map(ns, &proto_name_to_module_name/1)

    parts =
      case camelcase_prefix(ctx) do
        "" -> ns
        prefix -> [prefix | ns]
      end

    Enum.join(parts, ".")
  end

  defp camelcase_prefix(%{package_prefix: nil, module_prefix: nil, package: nil} = _ctx),
    do: ""

  defp camelcase_prefix(%{package_prefix: prefix, module_prefix: nil, package: package} = _ctx),
    do: proto_name_to_module_name(prepend_package_prefix(prefix, package))

  defp camelcase_prefix(%{module_prefix: module_prefix} = _ctx),
    do: proto_name_to_module_name(module_prefix)

  defp proto_name_to_module_name(name) when is_binary(name) do
    name
    |> String.split(".")
    |> Enum.map_join(".", &Macro.camelize/1)
  end

  @spec prepend_package_prefix(String.t() | nil, String.t() | nil) :: String.t()
  def prepend_package_prefix(prefix, package)
      when is_nil_or_nonempty_string(prefix) and is_nil_or_nonempty_string(package) do
    [prefix, package]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(".")
  end

  @spec options_to_str(%{optional(atom()) => atom() | integer() | String.t()}) :: String.t()
  def options_to_str(opts) when is_map(opts) do
    opts
    |> Enum.reject(fn {_key, val} -> val in [nil, false] end)
    |> Enum.sort()
    |> Enum.map_join(", ", fn {key, val} -> "#{key}: #{print(val)}" end)
  end

  defp print(atom) when is_atom(atom), do: inspect(atom)
  defp print(val), do: val

  @spec type_from_type_name(Context.t(), String.t()) :: String.t()
  def type_from_type_name(%Context{dep_type_mapping: mapping}, type_name)
      when is_binary(type_name) do
    # The doc says there's a situation where type_name begins without a `.`, but I never got that.
    # Handle that later.
    metadata =
      mapping[type_name] ||
        raise "There's something wrong to get #{type_name}'s type, please contact with the lib author."

    metadata[:type_name]
  end

  @spec descriptor_fun_body(desc :: struct()) :: String.t()
  def descriptor_fun_body(%mod{} = desc) do
    attributes =
      desc
      |> Map.from_struct()
      |> Enum.filter(fn {_key, val} -> not is_nil(val) end)

    struct!(mod, attributes)
    |> mod.encode()
    |> mod.decode()
    |> inspect(limit: :infinity)
  end

  @spec format(String.t()) :: String.t()
  def format(code) when is_binary(code) do
    formatted_code =
      code
      |> Code.format_string!(locals_without_parens: @locals_without_parens)
      |> IO.iodata_to_binary()

    # As neither Code.format_string!/2 nor protoc automatically adds a newline
    # at end of files, we must add ourselves if not present.
    if String.ends_with?(formatted_code, "\n"), do: formatted_code, else: formatted_code <> "\n"
  end

  @spec pad_comment(String.t(), non_neg_integer()) :: String.t()
  def pad_comment(comment, size) do
    padding = String.duplicate(" ", size)
    String.replace(comment, "\n", "\n" <> padding)
  end

  @spec version() :: String.t()
  def version do
    {:ok, value} = :application.get_key(:protobuf, :vsn)
    List.to_string(value)
  end
end

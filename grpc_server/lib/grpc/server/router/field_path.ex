defmodule GRPC.Server.Router.FieldPath do
  @moduledoc false

  @spec decode_pair({binary(), term()}, map()) :: map()
  def decode_pair({key, value}, acc) do
    parts = :binary.split(key, ".", [:global])
    assign_map(parts, value, acc)
  end

  defp assign_map(parts, value, acc) do
    {_, acc} =
      get_and_update_in(acc, Enum.map(parts, &Access.key(&1, %{})), fn
        prev when prev == %{} -> {prev, value}
        prev when is_list(prev) -> {prev, [value | prev]}
        prev -> {prev, [value, prev]}
      end)

    acc
  end
end

defmodule Protobuf.Any do
  @moduledoc false

  @spec type_url_to_module(String.t()) :: module()
  def type_url_to_module(type_url) when is_binary(type_url) do
    case type_url do
      "type.googleapis.com/" <> package_and_message ->
        package_and_message
        |> String.split(".")
        |> Enum.map(&Macro.camelize/1)
        |> Module.safe_concat()

      _other ->
        raise ArgumentError,
              "type_url must be in the form: type.googleapis.com/<package>.<message name>, " <>
                "got: #{inspect(type_url)}"
    end
  end
end

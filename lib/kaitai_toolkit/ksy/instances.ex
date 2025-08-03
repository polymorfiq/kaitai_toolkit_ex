defmodule KaitaiToolkit.Ksy.Instances do
  alias KaitaiToolkit.Ksy.Attribute
  @type t :: %{String.t() => Attribute.t()}

  @spec from_map!(map()) :: t()
  def from_map!(data) do
    data
    |> Enum.map(fn {key, data} -> {key, Attribute.from_map!(data)} end)
    |> Map.new()
  end
end

defmodule KaitaiToolkit do
  @moduledoc """
  Documentation for `KaitaiToolkit`.
  """

  defmacro use_kaitai_struct(contents) do
    YamlElixir.read_all_from_string!(contents) |> IO.inspect(label: "hmmm")
  end
end

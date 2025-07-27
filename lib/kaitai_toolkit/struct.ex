defmodule KaitaiToolkit.Struct do
  defmacro __using__(opts) do
    contents = Keyword.fetch!(opts, :contents)
    specs = YamlElixir.read_all_from_string!(contents)

    specs |> Enum.map(fn spec ->
      quote do
        unquote(Enum.map(spec["types"], fn {type_name, type_def} ->
          module_for_type(__CALLER__.module, spec, type_name, type_def)
        end))
      end
    end)
  end

  defp module_for_type(parent, _spec, type_name, _type_def) do
    quote do
      defmodule unquote(:"#{parent}.#{Macro.camelize(type_name)}") do
        @type t :: %{}

        @spec read!(stream :: KaitaiStruct.Stream.t()) :: t()
        def read!(stream) do
          :ok
        end
      end
    end
  end
end
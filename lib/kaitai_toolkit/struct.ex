defmodule KaitaiToolkit.Struct do
  @data_types [
    {~r/^u[1248](le|be)?$/, :integer},
    {~r/^s[1248](le|be)?$/, :integer},
    {~r/^f[48](le|be)?$/, :integer},
    {~r/^b1$/, :boolean},
    {~r/^b([0-9]+)$/, :integer},
    {~r/^bytes$/, :integer},
    {~r/^str(z)?$/, :binary},
    {~r/^string$/, :binary},
    {~r/^bool$/, :boolean},
    {~r/^boolean$/, :boolean},
    {~r/^struct$/, :term},
    {~r/^io$/, {:., [], [{:__aliases__, [], [:KaitaiStruct, :Stream]}, :t]}},
    {~r/^any$/, :term},
  ]

  defmacro __using__(opts) do
    contents = Keyword.fetch!(opts, :contents)
    specs = YamlElixir.read_all_from_string!(contents)

    specs |> Enum.map(fn spec ->
      quote do
        unquote(Enum.map(spec["types"], fn {type_name, type_def} ->
          my_module = module_for_type(__CALLER__.module, spec, type_name, type_def)
          my_module |> Macro.to_string() |> Code.format_string!() |> IO.puts()
          my_module
        end))
      end
    end)
  end

  defp module_for_type(parent, _spec, type_name, type_def) do
    quote do
      defmodule unquote(:"#{parent}.#{Macro.camelize(type_name)}") do
        defstruct unquote(Enum.map(type_def["seq"], fn attr -> String.to_atom(attr["id"]) end))

        @type t ::
          unquote({:%{}, [], Enum.map(type_def["seq"] || [], fn attr ->
            attr_type = attr["type"] || "byte"
            data_type = Enum.find(@data_types, fn {t_pattern, _} -> Regex.match?(t_pattern, attr_type) end)
            data_type = case data_type do
              nil -> Macro.camelize(attr_type)
              {_, t} -> t
            end

            {String.to_atom(attr["id"]), {data_type, [], []}}
          end)})

        @spec read!(stream :: KaitaiStruct.Stream.t()) :: t()
        def read!(stream) do
          :ok
        end

      end
    end
  end
end
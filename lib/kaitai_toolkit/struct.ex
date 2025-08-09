defmodule KaitaiToolkit.Struct do
  alias KaitaiToolkit.Generation
  alias KaitaiToolkit.Ksy

  defmacro __using__(opts) do
    ksy =
      cond do
        contents = Keyword.get(opts, :contents) ->
          {contents, _} = Code.eval_quoted(contents)
          Ksy.from_str!(contents)

        filename = Keyword.get(opts, :ksy) ->
          {filename, _} = Code.eval_quoted(filename)
          File.read!(filename) |> Ksy.from_str!()
      end

    generated = Generation.generate(ksy, root: __CALLER__.module)
#    generated |> Macro.to_string() |> Code.format_string!() |> IO.puts()

    generated
  end

  @spec parse_expr!(map(), term()) :: term()
  def parse_expr!(data, {:expr, expr}) do
    Map.fetch!(data, String.to_atom(expr))
  end

  def parse_expr!(_data, non_expr), do: non_expr
end

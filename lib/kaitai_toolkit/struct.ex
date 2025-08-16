defmodule KaitaiToolkit.Struct do
  alias KaitaiToolkit.Ksy.Expression
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
  def parse_expr!(data, {:expr, expr_str}) do
    parsed = expr_str |> Expression.lex() |> Expression.parse()

    case calculate_runtime_expr(data, parsed) do
      num when is_integer(num) -> {:literal, num}
      num when is_float(num) -> {:literal, num}
      val when is_binary(val) -> {:string, val}
    end
  end

  defp calculate_runtime_expr(_data, {:literal, val}), do: val
  defp calculate_runtime_expr(data, {:name, name}), do: Map.fetch!(data, String.to_atom(name))

  defp calculate_runtime_expr(data, {:multiply, a, b}) do
    calculate_runtime_expr(data, a) * calculate_runtime_expr(data, b)
  end

  defp calculate_runtime_expr(data, {:divide, a, b}) do
    val_a = calculate_runtime_expr(data, a)
    val_b = calculate_runtime_expr(data, b)

    if is_integer(val_a) || is_integer(val_b),
       do: div(a, b),
       else: a / b
  end

  defp calculate_runtime_expr(data, {:add, a, b}) do
    calculate_runtime_expr(data, a) + calculate_runtime_expr(data, b)
  end

  defp calculate_runtime_expr(data, {:subtract, a, b}) do
    calculate_runtime_expr(data, a) - calculate_runtime_expr(data, b)
  end

  defp calculate_runtime_expr(data, {:modulo, a, b}) do
    rem(calculate_runtime_expr(data, a), calculate_runtime_expr(data, b))
  end

  defp calculate_runtime_expr(data, {:negative, val}) do
    -calculate_runtime_expr(data, val)
  end
end

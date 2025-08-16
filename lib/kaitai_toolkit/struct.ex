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

  @spec parse_expr!(map(), term(), term()) :: term()
  def parse_expr!(data, self, {:expr, expr_str}) do
    ctx = %{data: data, self: self}
    parsed = expr_str |> Expression.lex() |> Expression.parse()

    calculate_runtime_expr(ctx, parsed)
  end

  defp calculate_runtime_expr(_ctx, val) when is_number(val), do: val
  defp calculate_runtime_expr(_ctx, val) when is_binary(val), do: val
  defp calculate_runtime_expr(_ctx, val) when is_boolean(val), do: val
  defp calculate_runtime_expr(ctx, vals) when is_list(vals), do: Enum.map(vals, &calculate_runtime_expr(ctx, &1))
  defp calculate_runtime_expr(ctx, {:name, name}), do: Map.fetch!(ctx.data, String.to_atom(name))

  defp calculate_runtime_expr(%{self: self} = ctx, {:property, :self, {:name, prop}}) do
    calculate_property_call(ctx, self, prop)
  end

  defp calculate_runtime_expr(%{self: self} = ctx, {:method_call, :self, {:name, method}, args}) do
    calculate_method_call(ctx, self, method, args)
  end

  defp calculate_runtime_expr(ctx, {:multiply, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_integer(a) and is_number(b) -> floor(a * b)
      {a, b} when is_number(a) and is_integer(b) -> floor (a * b)
      {a, b} when is_number(a) and is_number(b) -> a * b
    end
  end

  defp calculate_runtime_expr(ctx, {:divide, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_integer(a) and is_number(b) -> div(a, b)
      {a, b} when is_number(a) and is_integer(b) -> div(a, b)
      {a, b} when is_number(a) and is_number(b) -> a / b
    end
  end

  defp calculate_runtime_expr(ctx, {:add, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a + b
      {a, b} when is_binary(a) and is_binary(b) -> a <> b
    end
  end

  defp calculate_runtime_expr(ctx, {:subtract, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a - b
    end
  end

  defp calculate_runtime_expr(ctx, {:modulo, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Expression.modulo(a, b)
    end
  end

  defp calculate_runtime_expr(ctx, {:negative, val}) do
    val = calculate_runtime_expr(ctx, val)

    case val do
      val when is_number(val) -> -val
    end
  end

  defp calculate_runtime_expr(ctx, {:equals, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a == b
      {a, b} when is_boolean(a) and is_boolean(b) -> a == b
      {a, b} when is_binary(a) and is_binary(b) -> a == b
      {a, b} when is_list(a) and is_list(b) -> a == b
    end
  end

  defp calculate_runtime_expr(ctx, {:not_equals, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a != b
      {a, b} when is_boolean(a) and is_boolean(b) -> a != b
      {a, b} when is_binary(a) and is_binary(b) -> a != b
      {a, b} when is_list(a) and is_list(b) -> a != b
    end
  end

  defp calculate_runtime_expr(ctx, {:less_than, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a < b
    end
  end

  defp calculate_runtime_expr(ctx, {:greater_than, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a > b
    end
  end

  defp calculate_runtime_expr(ctx, {:gt_or_equals, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a >= b
    end
  end

  defp calculate_runtime_expr(ctx, {:lt_or_equals, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> a <= b
    end
  end

  defp calculate_runtime_expr(ctx, {:logical_and, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_boolean(a) and is_boolean(b) -> a && b
    end
  end

  defp calculate_runtime_expr(ctx, {:logical_or, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_boolean(a) and is_boolean(b) -> a || b
    end
  end

  defp calculate_runtime_expr(ctx, {:logical_not, a}) do
    val_a = calculate_runtime_expr(ctx, a)

    case val_a do
      a when is_boolean(a)  -> !a
    end
  end

  defp calculate_runtime_expr(ctx, {:bitwise_left, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Bitwise.<<<(a, b)
    end
  end

  defp calculate_runtime_expr(ctx, {:bitwise_right, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Bitwise.>>>(a, b)
    end
  end

  defp calculate_runtime_expr(ctx, {:bitwise_and, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Bitwise.band(a, b)
    end
  end

  defp calculate_runtime_expr(ctx, {:bitwise_or, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Bitwise.bor(a, b)
    end
  end

  defp calculate_runtime_expr(ctx, {:bitwise_xor, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_number(a) and is_number(b) -> Bitwise.bxor(a, b)
    end
  end

  defp calculate_property_call(_, int, "to_s") when is_integer(int), do: "#{int}"
  defp calculate_property_call(_, float, "to_i") when is_float(float), do: trunc(float)
  defp calculate_property_call(_, list, "length") when is_list(list), do: Enum.count(list)
  defp calculate_property_call(_, byte_array, "length") when is_binary(byte_array), do: byte_size(byte_array)
  defp calculate_property_call(_, {:string, str}, "length"), do: String.length(str)

  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-8"}]) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf8, :big})}}
  end

  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-16"}]) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf16, :big})}}
  end
  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-16LE"}]) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf16, :little})}}
  end
end

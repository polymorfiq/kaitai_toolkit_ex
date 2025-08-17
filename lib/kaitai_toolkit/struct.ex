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
  def parse_expr!(ctx, {:expr, expr_str}) do
    ctx = %{io: ctx.io, self: ctx.self, parents: ctx.parents}
    parsed = expr_str |> Expression.lex() |> Expression.parse()

    calculate_runtime_expr(ctx, parsed)
  end

  defp calculate_runtime_expr(%{io: io}, {:meta, :io}), do: io

  defp calculate_runtime_expr(%{self: self}, {:meta, :self}) when is_list(self),
    do: List.first(self)

  defp calculate_runtime_expr(%{parents: [parent | _]}, {:meta, :parent}), do: parent

  defp calculate_runtime_expr(%{parents: [_ | _] = parents}, {:meta, :root}),
    do: List.last(parents)

  defp calculate_runtime_expr(_ctx, val) when is_number(val), do: val
  defp calculate_runtime_expr(_ctx, val) when is_binary(val), do: val
  defp calculate_runtime_expr(_ctx, val) when is_boolean(val), do: val
  defp calculate_runtime_expr(_ctx, val) when is_binary(val), do: val

  defp calculate_runtime_expr(ctx, vals) when is_list(vals),
    do: Enum.map(vals, &calculate_runtime_expr(ctx, &1))

  defp calculate_runtime_expr(_ctx, {:string, str}), do: {:string, str}

  defp calculate_runtime_expr(ctx, vals) when is_list(vals),
    do: Enum.map(vals, &calculate_runtime_expr(ctx, &1))

  defp calculate_runtime_expr(ctx, {:name, name}),
    do: calculate_property_call(ctx, ctx.self, name)

  defp calculate_runtime_expr(ctx, {:property, val, {:name, prop}}) do
    calculated_val_expr = calculate_runtime_expr(ctx, val)
    calculate_property_call(%{ctx | self: calculated_val_expr}, calculated_val_expr, prop)
  end

  defp calculate_runtime_expr(ctx, {:method_call, val, {:name, method}, args}) do
    calculate_method_call(ctx, calculate_runtime_expr(ctx, val), method, args)
  end

  defp calculate_runtime_expr(ctx, {:multiply, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_integer(a) and is_number(b) -> floor(a * b)
      {a, b} when is_number(a) and is_integer(b) -> floor(a * b)
      {a, b} when is_number(a) and is_number(b) -> a * b
    end
  end

  defp calculate_runtime_expr(ctx, {:divide, a, b}) do
    val_a = calculate_runtime_expr(ctx, a)
    val_b = calculate_runtime_expr(ctx, b)

    case {val_a, val_b} do
      {a, b} when is_integer(a) and is_number(b) -> div(trunc(a), trunc(b))
      {a, b} when is_number(a) and is_integer(b) -> div(trunc(a), trunc(b))
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
      {{:string, a}, {:string, b}} -> a == b
      {nil, nil} -> true
      {_, nil} -> false
      {nil, _} -> false
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
      a when is_boolean(a) -> !a
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

  defp calculate_property_call(ctx, int, "to_s") when is_integer(int),
    do: calculate_method_call(ctx, int, "to_s", [])

  defp calculate_property_call(ctx, float, "to_i") when is_float(float),
    do: calculate_method_call(ctx, float, "to_i", [])

  defp calculate_property_call(ctx, bool, "to_i") when is_boolean(bool),
    do: calculate_method_call(ctx, bool, "to_i", [])

  defp calculate_property_call(ctx, list, "length") when is_list(list),
    do: calculate_method_call(ctx, list, "length", [])

  defp calculate_property_call(ctx, byte_array, "length") when is_binary(byte_array),
    do: calculate_method_call(ctx, byte_array, "length", [])

  defp calculate_property_call(ctx, {:string, str}, "length"),
    do: calculate_method_call(ctx, {:string, str}, "length", [])

  defp calculate_property_call(ctx, {:string, str}, "reverse"),
    do: calculate_method_call(ctx, {:string, str}, "reverse", [])

  defp calculate_property_call(ctx, {:string, str}, "to_i"),
    do: calculate_method_call(ctx, {:string, str}, "to_i", [])

  defp calculate_property_call(ctx, list, "first") when is_list(list),
    do: calculate_method_call(ctx, list, "first", [])

  defp calculate_property_call(ctx, list, "last") when is_list(list),
    do: calculate_method_call(ctx, list, "last", [])

  defp calculate_property_call(ctx, list, "size") when is_list(list),
    do: calculate_method_call(ctx, list, "size", [])

  defp calculate_property_call(ctx, list, "min") when is_list(list),
    do: calculate_method_call(ctx, list, "min", [])

  defp calculate_property_call(ctx, list, "max") when is_list(list),
    do: calculate_method_call(ctx, list, "max", [])

  defp calculate_property_call(ctx, stream, "eof") when is_pid(stream),
    do: calculate_method_call(ctx, stream, "eof", [])

  defp calculate_property_call(ctx, stream, "size") when is_pid(stream),
    do: calculate_method_call(ctx, stream, "size", [])

  defp calculate_property_call(ctx, stream, "pos") when is_pid(stream),
    do: calculate_method_call(ctx, stream, "pos", [])

  defp calculate_property_call(ctx, data, key_name) when is_map(data) do
    key = String.to_existing_atom(key_name)

    case Map.get(data, key) do
      {:instance, instance_fn} ->
        instance_fn.(ctx)

      {:value_instance, instance_fn} ->
        instance_fn.(ctx)

      val ->
        val
    end
  end

  defp calculate_method_call(_, int, "to_s", []) when is_integer(int) do
    {:string, "#{int}"}
  end

  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-8"}])
       when is_binary(byte_array) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf8, :big})}}
  end

  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-16"}])
       when is_binary(byte_array) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf16, :big})}}
  end

  defp calculate_method_call(_, byte_array, "to_s", [{:string, "UTF-16LE"}])
       when is_binary(byte_array) do
    {:string, {:unicode.characters_to_binary(byte_array, {:utf16, :little})}}
  end

  defp calculate_method_call(_, list, "length", []) when is_list(list) do
    Enum.count(list)
  end

  defp calculate_method_call(_, byte_array, "length", []) when is_binary(byte_array) do
    byte_size(byte_array)
  end

  defp calculate_method_call(_, {:string, str}, "length", []) do
    String.length(str)
  end

  defp calculate_method_call(_, float, "to_i", []) when is_float(float) do
    trunc(float)
  end

  defp calculate_method_call(_, bool, "to_i", []) when is_boolean(bool) do
    if bool, do: 1, else: 0
  end

  defp calculate_method_call(_, {:string, str}, "to_i", [radix]) do
    String.to_integer(str, radix)
  end

  defp calculate_method_call(_, {:string, str}, "to_i", []) do
    String.to_integer(str)
  end

  defp calculate_method_call(_, {:string, str}, "reverse", []) do
    String.reverse(str)
  end

  defp calculate_method_call(_, list, "first", []) when is_list(list) do
    List.first(list)
  end

  defp calculate_method_call(_, list, "last", []) when is_list(list) do
    List.last(list)
  end

  defp calculate_method_call(_, list, "size", []) when is_list(list) do
    Enum.count(list)
  end

  defp calculate_method_call(_, list, "min", []) when is_list(list) do
    Enum.min(list)
  end

  defp calculate_method_call(_, list, "max", []) when is_list(list) do
    Enum.max(list)
  end

  defp calculate_method_call(_, stream, "eof", []) when is_pid(stream) do
    KaitaiStruct.Stream.eof?(stream)
  end

  defp calculate_method_call(_, stream, "size", []) when is_pid(stream) do
    KaitaiStruct.Stream.size(stream)
  end

  defp calculate_method_call(_, stream, "pos", []) when is_pid(stream) do
    KaitaiStruct.Stream.pos(stream)
  end
end

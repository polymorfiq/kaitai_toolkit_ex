defmodule KaitaiToolkit.Ksy.Expression do
  @spec parse(lexed :: [term()], context :: map()) :: tuple()
  def parse(lexed, context \\ %{})

  def parse([cond_expr, :question_mark, true_expr, :colon | false_expr], ctx) do
    [{:ternary, parse([cond_expr], ctx), parse([true_expr], ctx), parse(false_expr, ctx)}]
    |> parse(ctx)
  end

  def parse([some_expr, :plus, other_expr], ctx) do
    [{:add, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([:minus, some_expr], ctx) do
    [{:negative, parse([some_expr], ctx)}] |> parse(ctx)
  end

  def parse([obj, :dot, method_name, {:parens, args}], ctx) do
    [
      {:method_call, parse([obj], ctx), parse([method_name], ctx),
       {:parens, parse_expr_list(args, ctx)}}
    ]
    |> parse(ctx)
  end

  def parse([obj, :dot, prop], ctx) do
    [{:prop_get, parse([obj], ctx), parse([prop], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :minus, other_expr], ctx) do
    [{:subtract, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :slash, other_expr], ctx) do
    [{:divide, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :star, other_expr], ctx) do
    [{:multiply, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :percent, other_expr], ctx) do
    [{:modulo, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :has_equality, other_expr], ctx) do
    [{:equals, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([:not, other_expr], ctx) do
    [{:logical_not, parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :and, other_expr], ctx) do
    [{:logical_and, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([some_expr, :or, other_expr], ctx) do
    [{:logical_or, parse([some_expr], ctx), parse([other_expr], ctx)}] |> parse(ctx)
  end

  def parse([{:integer, num}], ctx), do: [{:literal, num}] |> parse(ctx)
  def parse([{:float, num}], ctx), do: [{:literal, num}] |> parse(ctx)
  def parse([{:negative, {:integer, num}}], ctx), do: [{:literal, -num}] |> parse(ctx)
  def parse([{:negative, {:float, num}}], ctx), do: [{:literal, -num}] |> parse(ctx)
  def parse([:io], ctx), do: [{:meta, :io}] |> parse(ctx)
  def parse([:root], ctx), do: [{:meta, :root}] |> parse(ctx)
  def parse([:parent], ctx), do: [{:meta, :parent}] |> parse(ctx)
  def parse([{:brackets, exprs}], ctx), do: [{:list, parse_expr_list(exprs, ctx)}] |> parse(ctx)
  def parse([{:parens, {:literal, literal}}], ctx), do: [{:literal, literal}] |> parse(ctx)
  def parse([{:parens, expr}], ctx), do: {:parens, parse(expr, ctx)} |> parse_second_stage(ctx)
  def parse([name], ctx) when is_binary(name), do: [{:name, name}] |> parse(ctx)
  def parse([lexed], ctx), do: lexed |> parse_second_stage(ctx)
  def parse([], _ctx), do: :empty

  defp parse_second_stage(parsed, _ctx), do: parsed

  defp parse_expr_list(exprs, ctx, parsed \\ [])

  defp parse_expr_list([expr, :comma | remaining], ctx, parsed) do
    parse_expr_list(remaining, ctx, parsed ++ [parse([expr], ctx)])
  end

  defp parse_expr_list([expr], ctx, parsed), do: parsed ++ [parse([expr], ctx)]
  defp parse_expr_list([], _ctx, parsed), do: parsed

  @spec lex(str :: binary(), curr_word :: binary(), lexed :: [binary()], context :: map()) :: [
          term()
        ]
  def lex(
        str,
        word \\ <<>>,
        lexed \\ [],
        context \\ %{in_single_string: false, in_double_string: false}
      )

  def lex(<<>>, <<>>, lexed, _), do: lex_second_stage(lexed)
  def lex(<<>>, word, lexed, ctx), do: lex(<<>>, <<>>, lexed ++ [normalize_word(word)], ctx)

  def lex(<<char::binary-size(1), expr_str::binary>>, word, lexed, ctx) do
    cond do
      ctx.in_single_string && char == "'" ->
        lex(expr_str, <<>>, lexed ++ [{:string, word}], %{ctx | in_single_string: false})

      ctx.in_double_string && char == "\"" ->
        lex(expr_str, <<>>, lexed ++ [{:string, word}], %{ctx | in_double_string: false})

      ctx.in_double_string && char == "\\" ->
        # In the middle of a string - replace with escaped character
        <<escaped::binary-size(1), expr_str::binary>> = expr_str

        {char, expr_str} =
          cond do
            escaped == "a" -> {"\a", expr_str}
            escaped == "b" -> {"\b", expr_str}
            escaped == "t" -> {"\t", expr_str}
            escaped == "n" -> {"\n", expr_str}
            escaped == "v" -> {"\v", expr_str}
            escaped == "f" -> {"\f", expr_str}
            escaped == "r" -> {"\r", expr_str}
            escaped == "e" -> {"\e", expr_str}
            escaped == "\"" -> {"\"", expr_str}
            escaped == "'" -> {"'", expr_str}
            escaped == "\\\\" -> {"\\", expr_str}
            escaped == "u" -> {:unicode_code_point, "\\#{escaped}" <> expr_str}
            Regex.match?(~r|^[0-9]$|, escaped) -> {:ascii, "\\#{escaped}" <> expr_str}
          end

        {char, expr_str} =
          case {char, expr_str} do
            {:ascii, expr_str} ->
              [_, ascii_code_str, expr_str] = Regex.run(~r|^\\([0-9]{1, 3})(.*)|, expr_str)
              ascii_code = String.to_integer(ascii_code_str)
              {<<ascii_code>>, expr_str}

            {:unicode_code_point, expr_str} ->
              [_, utf_hex_str, expr_str] = Regex.run(~r|^\\u([0-9a-fA-F]{4})(.*)|, expr_str)
              {hex_code, ""} = Integer.parse(utf_hex_str)
              {:unicode.characters_to_binary(<<hex_code::integer-16>>, {:utf16, :big}), expr_str}

            _ ->
              {char, expr_str}
          end

        lex(expr_str, word <> char, lexed, ctx)

      ctx.in_double_string ->
        # In the middle of a string - just interpret the character literally
        lex(expr_str, word <> char, lexed, ctx)

      ctx.in_single_string ->
        # In the middle of a string - just interpret the character literally
        lex(expr_str, word <> char, lexed, ctx)

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) > 0 ->
        # Hit Whitespace - we have finished current word
        lex(expr_str, <<>>, lexed ++ [normalize_word(word)], ctx)

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) == 0 ->
        # Consecutive Whitespace - ignore it
        lex(expr_str, word, lexed, ctx)

      char == "'" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word)], %{ctx | in_single_string: true})

      char == "'" && String.length(word) == 0 ->
        lex(expr_str, word, lexed, %{ctx | in_single_string: true})

      char == "\"" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word)], %{ctx | in_double_string: true})

      char == "\"" && String.length(word) == 0 ->
        lex(expr_str, word, lexed, %{ctx | in_double_string: true})

      char == "[" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :open_square_bracket], ctx)

      char == "[" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:open_square_bracket], ctx)

      char == "]" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :close_square_bracket], ctx)

      char == "]" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:close_square_bracket], ctx)

      char == "(" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :open_parens], ctx)

      char == "(" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:open_parens], ctx)

      char == ")" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :close_parens], ctx)

      char == ")" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:close_parens], ctx)

      char == "+" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :plus], ctx)

      char == "+" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:plus], ctx)

      char == "-" && Regex.match?(~r|^[\-0-9\_\.e]+$|, word) ->
        # '-' in the middle of a scientific notation. It is part of the number
        lex(expr_str, word <> "-", lexed, ctx)

      char == "-" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :minus], ctx)

      char == "-" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:minus], ctx)

      char == "*" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :star], ctx)

      char == "*" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:star], ctx)

      char == "/" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :slash], ctx)

      char == "/" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:slash], ctx)

      char == "%" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :percent], ctx)

      char == "%" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:percent], ctx)

      char == "!" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :exclamation], ctx)

      char == "!" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:exclamation], ctx)

      char == "&" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :ampersand], ctx)

      char == "&" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:ampersand], ctx)

      char == "?" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :question_mark], ctx)

      char == "?" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:question_mark], ctx)

      char == ":" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :colon], ctx)

      char == ":" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:colon], ctx)

      char == "|" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :pipe], ctx)

      char == "|" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:pipe], ctx)

      char == "^" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :caret], ctx)

      char == "^" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:caret], ctx)

      char == "<" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :left_arrow], ctx)

      char == "<" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:left_arrow], ctx)

      char == ">" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :right_arrow], ctx)

      char == ">" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:right_arrow], ctx)

      char == "=" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :equals], ctx)

      char == "=" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:equals], ctx)

      char == "," && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :comma], ctx)

      char == "," && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:comma], ctx)

      char == "\\" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :backslash], ctx)

      char == "\\" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:backslash], ctx)

      char == "'" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :single_quote], ctx)

      char == "'" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:single_quote], ctx)

      char == "\"" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :double_quote], ctx)

      char == "\"" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:double_quote], ctx)

      char == "_" && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '_' in the middle of a number. Easier to read. Ignore it
        lex(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0b[01]+$|, word) ->
        # '_' in the middle of a binary number. Easier to read. Ignore it
        lex(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0o[01]+$|, word) ->
        # '_' in the middle of an octal number. Easier to read. Ignore it
        lex(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0x[01]+$|, word) ->
        # '_' in the middle of a hex number. Easier to read. Ignore it
        lex(expr_str, word, lexed, ctx)

      char == "." && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '.' in the middle of a number. It is part of the number.
        lex(expr_str, word <> ".", lexed, ctx)

      char == "." && String.length(word) > 0 ->
        # '.' not in the middle of a number - it is special
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :dot], ctx)

      char == "." && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:dot], ctx)

      true ->
        lex(expr_str, word <> char, lexed, ctx)
    end
  end

  defp normalize_word("true"), do: true
  defp normalize_word("false"), do: false

  defp normalize_word(word) do
    cond do
      word == "not" ->
        :not

      word == "and" ->
        :and

      word == "or" ->
        :or

      word == "_io" ->
        :io

      word == "_root" ->
        :root

      word == "_parent" ->
        :parent

      Regex.match?(~r|^0x[A-Fa-f0-9]+$|, word) ->
        "0x" <> hex_code = word
        {val, ""} = Integer.parse(hex_code, 16)
        {:integer, val}

      Regex.match?(~r|^0b[01]+$|, word) ->
        "0b" <> binary_form = word
        {val, ""} = Integer.parse(binary_form, 2)
        {:integer, val}

      Regex.match?(~r|^0o[0-7]+$|, word) ->
        "0o" <> octal_form = word
        {val, ""} = Integer.parse(octal_form, 8)
        {:integer, val}

      Regex.match?(~r|^[0-9]+$|, word) ->
        {val, ""} = Integer.parse(word)
        {:integer, val}

      Regex.match?(~r|^-[0-9]+$|, word) ->
        {val, ""} = Integer.parse(word)
        {:integer, val}

      Regex.match?(~r|^-[0-9]+\.[0-9]+$|, word) ->
        {val, ""} = Float.parse(word)
        {:float, val}

      Regex.match?(~r|^[0-9]+\.[0-9]+$|, word) ->
        {val, ""} = Float.parse(word)
        {:float, val}

      Regex.match?(~r|^-[0-9]+\.[0-9]+e[\-0-9]+$|, word) ->
        {val, ""} = Float.parse(word)
        {:float, val}

      Regex.match?(~r|^[0-9]+\.[0-9]+e[\-0-9]+$|, word) ->
        {val, ""} = Float.parse(word)
        {:float, val}

      true ->
        word
    end
  end

  defp lex_second_stage(lexed, lex_2 \\ [], ctx \\ %{parens_depth: 0, bracket_depth: 0})

  defp lex_second_stage([], lex_2, %{parens_depth: depth}) when depth > 0,
    do: raise("Expression finished with unclosed parenthesis - #{inspect(lex_2)}")

  defp lex_second_stage([], lex_2, %{bracket_depth: depth}) when depth > 0,
    do: raise("Expression finished with unclosed square brackets - #{inspect(lex_2)}")

  defp lex_second_stage([], lex_2, _), do: lex_2

  # Keep track of when parens was opened
  defp lex_second_stage([:open_parens | rest], lex_2, ctx) do
    lex_second_stage(rest, lex_2 ++ [{:open_parens, ctx.parens_depth + 1}], %{
      ctx
      | parens_depth: ctx.parens_depth + 1
    })
  end

  defp lex_second_stage([:close_parens | rest], lex_2, ctx) do
    split_by_parens_opening = Enum.split_while(lex_2, &(&1 != {:open_parens, ctx.parens_depth}))

    new_lex_2 =
      case split_by_parens_opening do
        {before_parens, parens_and_inside} ->
          inside_parens =
            parens_and_inside |> Enum.filter(&(&1 != {:open_parens, ctx.parens_depth}))

          before_parens ++ [{:parens, inside_parens}]

        _ ->
          raise "Found closing ')' but could not find matching '(' - #{inspect(lex_2 ++ [:close_parens])}"
      end

    lex_second_stage(rest, new_lex_2, %{ctx | parens_depth: ctx.parens_depth - 1})
  end

  # Keep track of when square bracket was opened
  defp lex_second_stage([:open_square_bracket | rest], lex_2, ctx) do
    lex_second_stage(rest, lex_2 ++ [{:open_square_bracket, ctx.bracket_depth + 1}], %{
      ctx
      | bracket_depth: ctx.bracket_depth + 1
    })
  end

  defp lex_second_stage([:close_square_bracket | rest], lex_2, ctx) do
    split_by_parens_opening =
      Enum.split_while(lex_2, &(&1 != {:open_square_bracket, ctx.bracket_depth}))

    new_lex_2 =
      case split_by_parens_opening do
        {before_brackets, brackets_and_inside} ->
          inside_brackets =
            brackets_and_inside |> Enum.filter(&(&1 != {:open_square_bracket, ctx.bracket_depth}))

          before_brackets ++ [{:brackets, inside_brackets}]

        _ ->
          raise "Found closing ']' but could not find matching '[' - #{inspect(lex_2 ++ [:close_square_bracket])}"
      end

    lex_second_stage(rest, new_lex_2, %{ctx | bracket_depth: ctx.bracket_depth - 1})
  end

  # Clean up double negatives (--)
  defp lex_second_stage([:minus, :minus | rest], lex_2, ctx),
    do: lex_second_stage([:plus | rest], lex_2, ctx)

  # Clean up double negatives (!!)
  defp lex_second_stage([:exclamation, :exclamation | rest], lex_2, ctx),
    do: lex_second_stage(rest, lex_2, ctx)

  defp lex_second_stage([:not, :not | rest], lex_2, ctx),
    do: lex_second_stage(rest, lex_2, ctx)

  defp lex_second_stage([:exclamation, true | rest], lex_2, ctx),
    do: lex_second_stage([false | rest], lex_2, ctx)

  defp lex_second_stage([:not, true | rest], lex_2, ctx),
    do: lex_second_stage([false | rest], lex_2, ctx)

  defp lex_second_stage([:exclamation, false | rest], lex_2, ctx),
    do: lex_second_stage([true | rest], lex_2, ctx)

  defp lex_second_stage([:not, false | rest], lex_2, ctx),
    do: lex_second_stage([true | rest], lex_2, ctx)

  # Transform != into :not_equals
  defp lex_second_stage([:exclamation, :equals | rest], lex_2, ctx),
    do: lex_second_stage([:not_equals | rest], lex_2, ctx)

  # Transforms <= into :lt_or_equals
  defp lex_second_stage([:left_arrow, :equals | rest], lex_2, ctx),
    do: lex_second_stage([:lt_or_equals | rest], lex_2, ctx)

  # Transforms << into :bit_shift_left
  defp lex_second_stage([:left_arrow, :left_arrow | rest], lex_2, ctx),
    do: lex_second_stage([:bit_shift_left | rest], lex_2, ctx)

  # Transforms >= into :gt_or_equals
  defp lex_second_stage([:right_arrow, :equals | rest], lex_2, ctx),
    do: lex_second_stage([:gt_or_equals | rest], lex_2, ctx)

  # Transforms >> into :bit_shift_right
  defp lex_second_stage([:right_arrow, :right_arrow | rest], lex_2, ctx),
    do: lex_second_stage([:bit_shift_right | rest], lex_2, ctx)

  defp lex_second_stage([:pipe | rest], lex_2, ctx),
    do: lex_second_stage([:bitwise_or | rest], lex_2, ctx)

  defp lex_second_stage([:caret | rest], lex_2, ctx),
    do: lex_second_stage([:bitwise_xor | rest], lex_2, ctx)

  # Transforms == into :has_equality
  defp lex_second_stage([:equals, :equals | rest], lex_2, ctx),
    do: lex_second_stage([:has_equality | rest], lex_2, ctx)

  # Passthrough for all unknown sequences
  defp lex_second_stage([word | rest], lex_2, ctx),
    do: lex_second_stage(rest, lex_2 ++ [word], ctx)
end

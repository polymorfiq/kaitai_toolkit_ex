defmodule KaitaiToolkit.Ksy.Expression do
  #
  # lex
  # The first stage of lexing transforms a list of characters into a stream of words. It also finds and segments string literals.
  # The second stage of lexing combines known multi-word combinations, into their more semantic forms. It also finds and segments Parenthesis and square brackets.
  #

  @spec lex(chars :: binary()) :: [term()]
  def lex(chars), do: chars |> lex_first_stage() |> lex_second_stage()

  #
  # lex_first_stage
  # Takes a stream of characters and transforms them into their semantic counterparts. Detects and preserves strings.
  #

  @spec lex_first_stage(str :: binary(), curr_word :: binary(), lexed :: [binary()], context :: map()) :: [
          term()
        ]
  defp lex_first_stage(
        str,
        word \\ <<>>,
        lexed \\ [],
        context \\ %{in_single_string: false, in_double_string: false}
      )

  defp lex_first_stage(<<>>, <<>>, lexed, _), do: lexed
  defp lex_first_stage(<<>>, word, lexed, ctx), do: lex_first_stage(<<>>, <<>>, lexed ++ [normalize_word(word)], ctx)

  defp lex_first_stage(<<char::binary-size(1), expr_str::binary>>, word, lexed, ctx) do
    cond do
      ctx.in_single_string && char == "'" ->
        lex_first_stage(expr_str, <<>>, lexed ++ [{:string, word}], %{ctx | in_single_string: false})

      ctx.in_double_string && char == "\"" ->
        lex_first_stage(expr_str, <<>>, lexed ++ [{:string, word}], %{ctx | in_double_string: false})

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

        lex_first_stage(expr_str, word <> char, lexed, ctx)

      ctx.in_double_string ->
        # In the middle of a string - just interpret the character literally
        lex_first_stage(expr_str, word <> char, lexed, ctx)

      ctx.in_single_string ->
        # In the middle of a string - just interpret the character literally
        lex_first_stage(expr_str, word <> char, lexed, ctx)

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) > 0 ->
        # Hit Whitespace - we have finished current word
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word)], ctx)

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) == 0 ->
        # Consecutive Whitespace - ignore it
        lex_first_stage(expr_str, word, lexed, ctx)

      char == "'" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word)], %{ctx | in_single_string: true})

      char == "'" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed, %{ctx | in_single_string: true})

      char == "\"" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word)], %{ctx | in_double_string: true})

      char == "\"" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed, %{ctx | in_double_string: true})

      char == "[" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :open_square_bracket], ctx)

      char == "[" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:open_square_bracket], ctx)

      char == "]" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :close_square_bracket], ctx)

      char == "]" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:close_square_bracket], ctx)

      char == "(" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :open_parens], ctx)

      char == "(" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:open_parens], ctx)

      char == ")" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :close_parens], ctx)

      char == ")" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:close_parens], ctx)

      char == "+" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :plus], ctx)

      char == "+" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:plus], ctx)

      char == "-" && Regex.match?(~r|^[\-0-9\_\.e]+$|, word) ->
        # '-' in the middle of a scientific notation. It is part of the number
        lex_first_stage(expr_str, word <> "-", lexed, ctx)

      char == "-" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :dash], ctx)

      char == "-" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:dash], ctx)

      char == "*" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :star], ctx)

      char == "*" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:star], ctx)

      char == "/" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :slash], ctx)

      char == "/" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:slash], ctx)

      char == "%" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :percent], ctx)

      char == "%" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:percent], ctx)

      char == "!" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :exclamation], ctx)

      char == "!" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:exclamation], ctx)

      char == "&" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :ampersand], ctx)

      char == "&" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:ampersand], ctx)

      char == "?" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :question_mark], ctx)

      char == "?" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:question_mark], ctx)

      char == ":" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :colon], ctx)

      char == ":" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:colon], ctx)

      char == "|" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :pipe], ctx)

      char == "|" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:pipe], ctx)

      char == "^" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :caret], ctx)

      char == "^" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:caret], ctx)

      char == "<" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :left_arrow], ctx)

      char == "<" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:left_arrow], ctx)

      char == ">" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :right_arrow], ctx)

      char == ">" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:right_arrow], ctx)

      char == "=" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :equals], ctx)

      char == "=" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:equals], ctx)

      char == "," && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :comma], ctx)

      char == "," && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:comma], ctx)

      char == "\\" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :backslash], ctx)

      char == "\\" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:backslash], ctx)

      char == "'" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :single_quote], ctx)

      char == "'" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:single_quote], ctx)

      char == "\"" && String.length(word) > 0 ->
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :double_quote], ctx)

      char == "\"" && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:double_quote], ctx)

      char == "_" && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '_' in the middle of a number. Easier to read. Ignore it
        lex_first_stage(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0b[01]+$|, word) ->
        # '_' in the middle of a binary number. Easier to read. Ignore it
        lex_first_stage(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0o[01]+$|, word) ->
        # '_' in the middle of an octal number. Easier to read. Ignore it
        lex_first_stage(expr_str, word, lexed, ctx)

      char == "_" && Regex.match?(~r|^0x[01]+$|, word) ->
        # '_' in the middle of a hex number. Easier to read. Ignore it
        lex_first_stage(expr_str, word, lexed, ctx)

      char == "." && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '.' in the middle of a number. It is part of the number.
        lex_first_stage(expr_str, word <> ".", lexed, ctx)

      char == "." && String.length(word) > 0 ->
        # '.' not in the middle of a number - it is special
        lex_first_stage(expr_str, <<>>, lexed ++ [normalize_word(word), :dot], ctx)

      char == "." && String.length(word) == 0 ->
        lex_first_stage(expr_str, word, lexed ++ [:dot], ctx)

      true ->
        lex_first_stage(expr_str, word <> char, lexed, ctx)
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

  #
  # lex_second_stage
  # Combines multi-word expressions into their more-semantic counterparts. Also groups expressions in parenthesis and square brackets
  # Say for example, transforms (:exclamation + :equals) into (:not_equals)
  #

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

  # Transforms == into :is_equal
  defp lex_second_stage([:equals, :equals | rest], lex_2, ctx),
    do: lex_second_stage([:is_equal | rest], lex_2, ctx)

  # Passthrough for all unknown sequences
  defp lex_second_stage([word | rest], lex_2, ctx),
    do: lex_second_stage(rest, lex_2 ++ [word], ctx)


 #
 # parse
 # The goal of parsing is to take a list of semantically-enriched words and transform them into an unambiguous AST
 # The first stage of parsing groups words into semantically-equivalent tuples that helps preserve order-of-operations rules in future steps.
 #

  @spec parse(lexed :: [term()]) :: tuple()
  def parse(lexed), do: lexed |> parse_pemdas() |> parse_first_stage() |> parse_second_stage()

  @spec parse_first_stage(lexed :: [term()], context :: map()) :: tuple()
  defp parse_first_stage(lexed, ctx \\ %{})

  defp parse_first_stage([obj, :dot, method_name, {:parens, args}], ctx) do
    [
      {:method_call, parse_first_stage([obj], ctx), parse_first_stage([method_name], ctx), parse_first_stage_list(args, ctx)}
    ]
    |> parse_first_stage(ctx)
  end

  defp parse_first_stage([obj, :dot, prop], ctx) do
    [{:prop_get, parse_first_stage([obj], ctx), parse_first_stage([prop], ctx)}] |> parse_first_stage(ctx)
  end

  defp parse_first_stage([some_expr, :is_equal, other_expr], ctx) do
    [{:equals, parse_first_stage([some_expr], ctx), parse_first_stage([other_expr], ctx)}] |> parse_first_stage(ctx)
  end

  defp parse_first_stage([condition, :question_mark, if_true, :colon, if_false], ctx), do: {:ternary, parse_first_stage([condition], ctx), parse_first_stage([if_true], ctx), parse_first_stage([if_false], ctx)}
  defp parse_first_stage([{:multiply, a, b}], ctx), do: {:multiply, parse_first_stage([a], ctx), parse_first_stage([b], ctx)}
  defp parse_first_stage([{:divide, a, b}], ctx), do: {:divide, parse_first_stage([a], ctx), parse_first_stage([b], ctx)}
  defp parse_first_stage([{:add, a, b}], ctx), do: {:add, parse_first_stage([a], ctx), parse_first_stage([b], ctx)}
  defp parse_first_stage([{:subtract, a, b}], ctx), do: {:subtract, parse_first_stage([a], ctx), parse_first_stage([b], ctx)}
  defp parse_first_stage([{:integer, num}], ctx), do: [{:literal, num}] |> parse_first_stage(ctx)
  defp parse_first_stage([{:float, num}], ctx), do: [{:literal, num}] |> parse_first_stage(ctx)
  defp parse_first_stage([{:negative, {:integer, num}}], ctx), do: [{:literal, -num}] |> parse_first_stage(ctx)
  defp parse_first_stage([{:negative, {:float, num}}], ctx), do: [{:literal, -num}] |> parse_first_stage(ctx)
  defp parse_first_stage([:io], ctx), do: [{:meta, :io}] |> parse_first_stage(ctx)
  defp parse_first_stage([:root], ctx), do: [{:meta, :root}] |> parse_first_stage(ctx)
  defp parse_first_stage([:parent], ctx), do: [{:meta, :parent}] |> parse_first_stage(ctx)
  defp parse_first_stage([{:brackets, items}], ctx), do: [{:array, parse_first_stage_list(items, ctx)}] |> parse_first_stage(ctx)
  defp parse_first_stage([{:parens, [single_expr]}], ctx), do: [single_expr] |> parse_first_stage(ctx)

  defp parse_first_stage([{type, exprs}], ctx) when is_list(exprs),
       do: {type, Enum.map(exprs, & parse_first_stage([&1], ctx))}

  defp parse_first_stage([name], ctx) when is_binary(name), do: [{:name, name}] |> parse_first_stage(ctx)
  defp parse_first_stage([parsed], _ctx), do: parsed
  defp parse_first_stage([], _ctx), do: :empty

  defp parse_first_stage_list(exprs, ctx, parsed \\ [])

  defp parse_first_stage_list([expr, :comma | remaining], ctx, parsed) do
    parse_first_stage_list(remaining, ctx, parsed ++ [parse_first_stage([expr], ctx)])
  end

  defp parse_first_stage_list([expr], ctx, parsed), do: parsed ++ [parse_first_stage([expr], ctx)]
  defp parse_first_stage_list([], _ctx, parsed), do: parsed

  #
  # parse_first_stage
  # Further simplifies redundant tuples, such as auto-calculating math down to constants
  # At this point we have an AST, so this simplifies the AST
  #
  defp parse_second_stage(parsed, ctx \\ %{})
  defp parse_second_stage({:add, {:literal, a}, {:literal, b}}, _ctx), do: {:literal, a + b}
  defp parse_second_stage({:add, {:string, a}, {:string, b}}, _ctx), do: {:string, a <> b}
  defp parse_second_stage({:add, a, b} = orig, _ctx) do
    parsed = {:add, parse_second_stage(a), parse_second_stage(b)}
    if parsed == orig, do: orig, else: parse_second_stage(parsed)
  end

  defp parse_second_stage({:subtract, {:literal, a}, {:literal, b}}, _ctx), do: {:literal, a - b}
  defp parse_second_stage({:subtract, a, b} = orig, _ctx) do
    parsed = {:subtract, parse_second_stage(a), parse_second_stage(b)}
    if parsed == orig, do: orig, else: parse_second_stage(parsed)
  end

  defp parse_second_stage({:multiply, {:literal, a}, {:literal, b}}, _ctx), do: {:literal, a * b}
  defp parse_second_stage({:multiply, a, b} = orig, _ctx) do
    parsed = {:multiply, parse_second_stage(a), parse_second_stage(b)}
    if parsed == orig, do: orig, else: parse_second_stage(parsed)
  end

  defp parse_second_stage({:divide, {:literal, a}, {:literal, b}}, _ctx), do: {:literal, a / b}
  defp parse_second_stage({:divide, a, b} = orig, _ctx) do
    parsed = {:divide, parse_second_stage(a), parse_second_stage(b)}
    if parsed == orig, do: orig, else: parse_second_stage(parsed)
  end

  defp parse_second_stage({:modulo, {:literal, a}, {:literal, b}}, _ctx), do: {:literal, rem(a, b)}
  defp parse_second_stage({:modulo, a, b} = orig, _ctx) do
    parsed = {:modulo, parse_second_stage(a), parse_second_stage(b)}
    if parsed == orig, do: orig, else: parse_second_stage(parsed)
  end

  defp parse_second_stage({:parens, exprs}, _ctx), do: {:parens, Enum.map(exprs, &parse_second_stage/1)}
  defp parse_second_stage({:brackets, exprs}, _ctx), do: {:brackets, Enum.map(exprs, &parse_second_stage/1)}
  defp parse_second_stage(other, _ctx), do: other

  #
  # parse_pemdas
  # Designed to go through and ensure proper order of math operations
  #
  @spec parse_pemdas(lexed :: [term()]) :: [term()]
  defp parse_pemdas(lexed) do
    lexed |> parse_pemdas_normalize() |> parse_pemdas_process(:star, :multiply) |> parse_pemdas_process(:slash, :divide) |> parse_pemdas_process(:plus, :add) |> parse_pemdas_process(:dash, :subtract)
  end

  # We want to properly handle negatives, like '5 * -2' - so let's first normalize those so they're not confusing
  defp parse_pemdas_normalize(lexed, seen \\ [])

  defp parse_pemdas_normalize([{type, [_ | _] = exprs} | rest], seen),
       do: parse_pemdas_normalize(rest, seen ++ [{type, parse_pemdas_normalize(exprs)}])

  defp parse_pemdas_normalize([:dash, expr | rest], []),
       do: parse_pemdas_normalize([{:negative, expr} | rest], [])

  defp parse_pemdas_normalize([op, :dash, expr | rest], seen) when is_atom(op),
       do: parse_pemdas_normalize([op, {:negative, expr} | rest], seen)

  defp parse_pemdas_normalize([expr | rest], seen),
       do: parse_pemdas_normalize(rest, seen ++ [expr])

  defp parse_pemdas_normalize([], seen), do: seen

  @math_operands [:integer, :float, :string]
  @math_ops [:multiply, :divide, :add, :subtract]
  defp parse_pemdas_process(lexed, symbol, op, seen \\ [])

  defp parse_pemdas_process([some, middle_expr, other | rest], symbol, op, seen) when middle_expr == symbol do
    if math_obj?(some) && math_obj?(other) do
      parse_pemdas_process([{op, some, other} | rest], symbol, op, seen)
    else
      parse_pemdas_process([middle_expr, other | rest], symbol, op, seen ++ [some])
    end
  end

  defp parse_pemdas_process([{math_op, a, b} | rest], symbol, op, seen) when math_op in @math_ops,
       do: parse_pemdas_process(rest, symbol, op, seen ++ [{math_op, parse_pemdas_process([a], symbol, op) |> List.first(), parse_pemdas_process([b], symbol, op) |> List.first()}])

  defp parse_pemdas_process([{:parens, exprs} | rest], symbol, op, seen),
       do: parse_pemdas_process(rest, symbol, op, seen ++ [{:parens, parse_pemdas_process(exprs, symbol, op)}])

  defp parse_pemdas_process([parsed | rest], symbol, op, seen), do: parse_pemdas_process(rest, symbol, op, seen ++ [parsed])
  defp parse_pemdas_process([], _, _, seen), do: seen

  defp math_obj?({:parens, _}), do: true
  defp math_obj?({op, _, _}) when op in @math_ops, do: true
  defp math_obj?({operand, _}) when operand in @math_operands, do: true
  defp math_obj?(operand) when is_binary(operand), do: true
  defp math_obj?(_), do: false
end

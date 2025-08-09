defmodule KaitaiToolkit.Ksy.Expression do
  @spec lex(binary(), binary(), [binary()]) :: [binary()]
  def lex(str, word \\ <<>>, lexed \\ [])
  def lex(<<>>, <<>>, lexed), do: lex_second_stage(lexed)
  def lex(<<>>, word, lexed), do: lex(<<>>, <<>>, lexed ++ [normalize_word(word)])

  def lex(<<char::binary-size(1), expr_str::binary>>, word, lexed) do
    cond do
      Regex.match?(~r|^[\s]+$|, char) && String.length(word) > 0 ->
        # Hit Whitespace - we have finished current word
        lex(expr_str, <<>>, lexed ++ [normalize_word(word)])

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) == 0 ->
        # Consecutive Whitespace - ignore it
        lex(expr_str, word, lexed)

      char == "[" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :open_square_bracket])

      char == "[" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:open_square_bracket])

      char == "]" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :close_square_bracket])

      char == "]" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:close_square_bracket])

      char == "(" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :open_parens])

      char == "(" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:open_parens])

      char == ")" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :close_parens])

      char == ")" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:close_parens])

      char == "+" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :plus])

      char == "+" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:plus])

      char == "-" && Regex.match?(~r|^[\-0-9\_\.e]+$|, word) ->
        # '-' in the middle of a scientific notation. It is part of the number
        lex(expr_str, word <> "-", lexed)

      char == "-" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :minus])

      char == "-" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:minus])

      char == "*" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :star])

      char == "*" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:star])

      char == "/" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :slash])

      char == "/" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:slash])

      char == "%" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :percent])

      char == "%" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:percent])

      char == "!" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :exclamation])

      char == "!" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:exclamation])

      char == "&" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :ampersand])

      char == "&" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:ampersand])

      char == "?" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :question_mark])

      char == "?" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:question_mark])

      char == ":" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :colon])

      char == ":" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:colon])

      char == "|" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :pipe])

      char == "|" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:pipe])

      char == "^" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :caret])

      char == "^" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:caret])

      char == "<" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :left_arrow])

      char == "<" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:left_arrow])

      char == ">" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :right_arrow])

      char == ">" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:right_arrow])

      char == "=" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :equals])

      char == "=" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:equals])

      char == "," && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :comma])

      char == "," && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:comma])

      char == "\\" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :backslash])

      char == "\\" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:backslash])

      char == "'" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :single_quote])

      char == "'" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:single_quote])

      char == "\"" && String.length(word) > 0 ->
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :double_quote])

      char == "\"" && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:double_quote])

      char == "_" && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '_' in the middle of a number. Easier to read. Ignore it
        lex(expr_str, word, lexed)

      char == "_" && Regex.match?(~r|^0b[01]+$|, word) ->
        # '_' in the middle of a binary number. Easier to read. Ignore it
        lex(expr_str, word, lexed)

      char == "_" && Regex.match?(~r|^0o[01]+$|, word) ->
        # '_' in the middle of an octal number. Easier to read. Ignore it
        lex(expr_str, word, lexed)

      char == "_" && Regex.match?(~r|^0x[01]+$|, word) ->
        # '_' in the middle of a hex number. Easier to read. Ignore it
        lex(expr_str, word, lexed)

      char == "." && Regex.match?(~r|^[\-0-9\_\.]+$|, word) ->
        # '.' in the middle of a number. It is part of the number.
        lex(expr_str, word <> ".", lexed)

      char == "." && String.length(word) > 0 ->
        # '.' not in the middle of a number - it is special
        lex(expr_str, <<>>, lexed ++ [normalize_word(word), :dot])

      char == "." && String.length(word) == 0 ->
        lex(expr_str, word, lexed ++ [:dot])

      true ->
        lex(expr_str, word <> char, lexed)
    end
  end

  defp normalize_word("true"), do: :true
  defp normalize_word("false"), do: :false
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

      true -> word
    end
  end

  defp lex_second_stage(lexed, lex_2 \\ [])
  defp lex_second_stage([], lex_2), do: lex_2

  # Transform minus (integer) into negative number
  defp lex_second_stage([:minus, {:integer, num} | rest], lex_2),
    do: lex_second_stage([{:integer, -num} | rest], lex_2)

  # Transform minus (float) into negative number
  defp lex_second_stage([:minus, {:float, num} | rest], lex_2),
      do: lex_second_stage([{:float, -num} | rest], lex_2)

  # Double negation is nothing - ignore it
  defp lex_second_stage([:exclamation, :exclamation | rest], lex_2),
       do: lex_second_stage(rest, lex_2)

  defp lex_second_stage([:not, :not | rest], lex_2),
       do: lex_second_stage(rest, lex_2)

  defp lex_second_stage([:exclamation, :true | rest], lex_2),
       do: lex_second_stage([:false | rest], lex_2)

  defp lex_second_stage([:not, :true | rest], lex_2),
       do: lex_second_stage([:false | rest], lex_2)

  defp lex_second_stage([:exclamation, :false | rest], lex_2),
       do: lex_second_stage([:true | rest], lex_2)

  defp lex_second_stage([:not, :false | rest], lex_2),
       do: lex_second_stage([:true | rest], lex_2)

  # Transform != into :not_equals
  defp lex_second_stage([:exclamation, :equals | rest], lex_2),
       do: lex_second_stage([:not_equals | rest], lex_2)

  # Transforms <= into :lt_or_equals
  defp lex_second_stage([:left_arrow, :equals | rest], lex_2),
       do: lex_second_stage([:lt_or_equals | rest], lex_2)

  # Transforms << into :bit_shift_left
  defp lex_second_stage([:left_arrow, :left_arrow | rest], lex_2),
       do: lex_second_stage([:bit_shift_left | rest], lex_2)

  # Transforms >= into :gt_or_equals
  defp lex_second_stage([:right_arrow, :equals | rest], lex_2),
       do: lex_second_stage([:gt_or_equals | rest], lex_2)

  # Transforms >> into :bit_shift_right
  defp lex_second_stage([:right_arrow, :right_arrow | rest], lex_2),
       do: lex_second_stage([:bit_shift_right | rest], lex_2)

  # Transforms == into :has_equality
  defp lex_second_stage([:equals, :equals | rest], lex_2),
       do: lex_second_stage([:has_equality | rest], lex_2)

  # Passthrough for all unknown sequences
  defp lex_second_stage([word | rest], lex_2),
       do: lex_second_stage(rest, lex_2 ++ [word])
end
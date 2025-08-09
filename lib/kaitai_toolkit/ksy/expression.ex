defmodule KaitaiToolkit.Ksy.Expression do
  @spec lex(binary(), binary(), [binary()]) :: [binary()]
  def lex(str, word \\ <<>>, lexed \\ [])
  def lex(<<>>, <<>>, lexed), do: lexed
  def lex(<<>>, word, lexed), do: lex(<<>>, <<>>, lexed ++ [word])

  def lex(<<char::binary-size(1), expr_str::binary>>, word, lexed) do
    cond do
      Regex.match?(~r|^[\s]+$|, char) && String.length(word) > 0 ->
        # Hit Whitespace - we have finished current word
        lex(expr_str, <<>>, lexed ++ [word])

      Regex.match?(~r|^[\s]+$|, char) && String.length(word) == 0 ->
        # Consecutive Whitespace - ignore it
        lex(expr_str, word, lexed)

      Regex.match?(~r|^[\_]+$|, char) && Regex.match?(~r|^[0-9\_\.]+$|, word) ->
        # '_' in the middle of a number. Easier to read. Ignore it
        lex(expr_str, word, lexed)

      true ->
        lex(expr_str, word <> char, lexed)
    end
  end
end
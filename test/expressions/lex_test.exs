defmodule KaitaiToolkitTest.Expressions.LexTest do
  use ExUnit.Case
  doctest KaitaiToolkit.Ksy.Expression
  alias KaitaiToolkit.Ksy.Expression

  test "parses a basic sentence" do
    assert ["if", "something", "=", "1"] = Expression.lex("if something = 1")
  end

  test "ignors numeric _" do
    assert ["123", "+", "456.789"] = Expression.lex("1_23 + 45_6.7_8_9")
  end
end
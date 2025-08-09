defmodule KaitaiToolkitTest.Expressions.LexTest do
  use ExUnit.Case
  doctest KaitaiToolkit.Ksy.Expression
  alias KaitaiToolkit.Ksy.Expression

  test "parses a basic sentence" do
    assert ["if", "something", :equals, {:integer, 1}] = Expression.lex("if something = 1")
  end

  test "ignores numeric _" do
    assert [{:integer, 123}, :plus, {:float, 456.789}] = Expression.lex("1_23 + 45_6.7_8_9")
  end

  test "handles scientific notation" do
    assert [{:float, a}, :star, {:float, b}] = Expression.lex("1.23e-1 * 1.23e2")
    assert_in_delta(a, 0.123, 0.001)
    assert_in_delta(b, 123.0, 0.001)

    assert [{:float, a}, :star, {:float, b}] = Expression.lex("1.23e-1 * -1.23e2")
    assert_in_delta(a, 0.123, 0.001)
    assert_in_delta(b, -123.0, 0.001)
  end

  test "handles negative numbers" do
    assert [{:integer, -123}, :plus, {:float, -456.789}] = Expression.lex("-1_23 + -45_6.7_8_9")
  end

  test "handles booleans" do
    assert ["abc", :not_equals, :true] = Expression.lex("abc != true")
    assert ["abc", :has_equality, :false] = Expression.lex("abc == false")
  end

  test "handles bit manipulations" do
    assert ["abc", :bit_shift_right, {:integer, 2}, :has_equality, {:integer, 128}] = Expression.lex("abc >> 2 == 128")
    assert ["abc", :bit_shift_left, {:integer, 2}, :has_equality, {:integer, 128}] = Expression.lex("abc << 2 == 128")
    assert ["a", :ampersand, {:integer, 1}, :has_equality, :true] = Expression.lex("a & 1 == true")
    assert ["a", :pipe, {:integer, 1}, :has_equality, :true] = Expression.lex("a | 1 == true")
    assert ["a", :caret, {:integer, 1}, :has_equality, :true] = Expression.lex("a ^ 1 == true")
  end

  test "handles logical operators" do
    assert ["a", :and, :true] = Expression.lex("a and true")
    assert ["a", :and, :false] = Expression.lex("a and not true")
    assert ["a", :and, :not, "b"] = Expression.lex("a and not b")
    assert ["a", :or, :not, "b"] = Expression.lex("a or not b")
  end

  test "handles ternary operators" do
    assert ["a", :question_mark, "b", :colon, "c"] = Expression.lex("a ? b : c")
  end

  test "handles method calls" do
    assert ["a", :dot, "method", :open_parens, {:integer, 123}, :close_parens] = Expression.lex("a.method(123)")
  end

  test "handles arrows" do
    assert ["abc", :left_arrow, {:integer, 123}] = Expression.lex("abc < 123")
    assert ["abc", :right_arrow, {:integer, 123}] = Expression.lex("abc > 123")
    assert ["abc", :lt_or_equals, {:integer, 123}] = Expression.lex("abc <= 123")
    assert ["abc", :gt_or_equals, {:integer, 123}] = Expression.lex("abc >= 123")
  end

  test "handles brackets" do
    assert [:open_square_bracket, {:integer, 1}, :close_square_bracket] = Expression.lex("[1]")
    assert [:open_square_bracket, {:integer, 1}, :comma, "abc", :comma, {:integer, 2}, :close_square_bracket] = Expression.lex("[1, abc, 2]")
  end

  test "handles single quotes" do
    assert [:single_quote, {:integer, 1}, :single_quote] = Expression.lex("'1'")
    assert [:single_quote, "abc", :single_quote] = Expression.lex("'abc'")
  end

  test "handles double quotes" do
    assert [:double_quote, {:integer, 1}, :double_quote] = Expression.lex(~s|"1"|)
    assert [:double_quote, "abc", :double_quote] = Expression.lex(~s|"abc"|)
  end

  test "handles backslashes" do
    assert [:double_quote, :backslash, "a", :backslash, "b", :double_quote] = Expression.lex(~s|"\\a\\b"|)
  end

  test "handles private variables" do
    assert [:io] = Expression.lex(~s|_io|)
    assert [:root] = Expression.lex(~s|_root|)
    assert [:parent] = Expression.lex(~s|_parent|)
  end

  test "handles negation of literals" do
    assert [:true] = Expression.lex("!false")
    assert [:false] = Expression.lex("!true")
    assert [:false] = Expression.lex("!!false")
    assert [:true] = Expression.lex("!!true")
    assert [:true] = Expression.lex("!!!false")
    assert [:false] = Expression.lex("!!!true")
  end
end
defmodule KaitaiToolkitTest.Expressions.LexTest do
  use ExUnit.Case
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

    assert [{:float, a}, :star, :minus, {:float, b}] = Expression.lex("1.23e-1 * -1.23e2")
    assert_in_delta(a, 0.123, 0.001)
    assert_in_delta(b, 123.0, 0.001)
  end

  test "handles negative numbers" do
    assert [:minus, {:integer, 123}, :plus, :minus, {:float, float_val}] =
             Expression.lex("-1_23 + -45_6.7_8_9")

    assert_in_delta(float_val, 456.789, 0.01)
  end

  test "handles booleans" do
    assert ["abc", :not_equals, true] = Expression.lex("abc != true")
    assert ["abc", :has_equality, false] = Expression.lex("abc == false")
  end

  test "handles bit manipulations" do
    assert ["abc", :bit_shift_right, {:integer, 2}, :has_equality, {:integer, 128}] =
             Expression.lex("abc >> 2 == 128")

    assert ["abc", :bit_shift_left, {:integer, 2}, :has_equality, {:integer, 128}] =
             Expression.lex("abc << 2 == 128")

    assert ["a", :ampersand, {:integer, 1}, :has_equality, true] = Expression.lex("a & 1 == true")

    assert ["a", :bitwise_or, {:integer, 1}, :has_equality, true] =
             Expression.lex("a | 1 == true")

    assert ["a", :bitwise_xor, {:integer, 1}, :has_equality, true] =
             Expression.lex("a ^ 1 == true")
  end

  test "handles logical operators" do
    assert ["a", :and, true] = Expression.lex("a and true")
    assert ["a", :and, false] = Expression.lex("a and not true")
    assert ["a", :and, :not, "b"] = Expression.lex("a and not b")
    assert ["a", :or, :not, "b"] = Expression.lex("a or not b")
  end

  test "handles ternary operators" do
    assert ["a", :question_mark, "b", :colon, "c"] = Expression.lex("a ? b : c")
  end

  test "handles parenthesis correctly" do
    assert ["a", :dot, "method", {:parens, []}] = Expression.lex("a.method()")
    assert ["a", :dot, "method", {:parens, [{:integer, 123}]}] = Expression.lex("a.method(123)")

    assert [
             "a",
             :dot,
             "method",
             {:parens,
              [{:integer, 123}, :star, {:parens, [{:integer, 456}, :minus, {:integer, 234}]}]}
           ] = Expression.lex("a.method(123 * (456 - 234))")
  end

  test "handles arrows" do
    assert ["abc", :left_arrow, {:integer, 123}] = Expression.lex("abc < 123")
    assert ["abc", :right_arrow, {:integer, 123}] = Expression.lex("abc > 123")
    assert ["abc", :lt_or_equals, {:integer, 123}] = Expression.lex("abc <= 123")
    assert ["abc", :gt_or_equals, {:integer, 123}] = Expression.lex("abc >= 123")
  end

  test "handles brackets" do
    assert [{:brackets, [{:integer, 1}]}] = Expression.lex("[1]")

    assert [{:brackets, [{:integer, 1}, :comma, "abc", :comma, {:integer, 2}]}] = Expression.lex("[1, abc, 2]")
  end

  test "handles single quotes" do
    assert [{:string, "1"}] = Expression.lex("'1'")
    assert [{:string, "abc"}] = Expression.lex("'abc'")
  end

  test "handles double quotes" do
    assert [{:string, "1"}] = Expression.lex(~s|"1"|)
    assert [{:string, "abc"}] = Expression.lex(~s|"abc"|)
  end

  test "single quotes ignore backslashes" do
    assert [{:string, ~s|app\\"le"\\np"i"e|}] = Expression.lex(~s|'app\\"le"\\np"i"e'|)
  end

  test "double quotes interpret backslashes" do
    assert [{:string, ~s|app"le"\np"i"e|}] = Expression.lex(~s|"app\\"le\\"\\np\\"i\\"e"|)
  end

  test "handles private variables" do
    assert [:io] = Expression.lex(~s|_io|)
    assert [:root] = Expression.lex(~s|_root|)
    assert [:parent] = Expression.lex(~s|_parent|)
  end

  test "handles negation of literals" do
    assert [true] = Expression.lex("!false")
    assert [false] = Expression.lex("!true")
    assert [false] = Expression.lex("!!false")
    assert [true] = Expression.lex("!!true")
    assert [true] = Expression.lex("!!!false")
    assert [false] = Expression.lex("!!!true")
  end
end

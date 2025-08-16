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

    assert [{:float, a}, :star, :dash, {:float, b}] = Expression.lex("1.23e-1 * -1.23e2")
    assert_in_delta(a, 0.123, 0.001)
    assert_in_delta(b, 123.0, 0.001)
  end

  test "handles negative numbers" do
    assert [:dash, {:integer, 123}, :plus, :dash, {:float, float_val}] =
             Expression.lex("-1_23 + -45_6.7_8_9")

    assert_in_delta(float_val, 456.789, 0.01)
  end

  test "handles booleans" do
    assert ["abc", :is_not_equal, true] = Expression.lex("abc != true")
    assert ["abc", :is_equal, false] = Expression.lex("abc == false")
  end

  test "handles bit manipulations" do
    assert ["abc", :bit_shift_right, {:integer, 2}, :is_equal, {:integer, 128}] =
             Expression.lex("abc >> 2 == 128")

    assert ["abc", :bit_shift_left, {:integer, 2}, :is_equal, {:integer, 128}] =
             Expression.lex("abc << 2 == 128")

    assert ["a", :ampersand, {:integer, 1}, :is_equal, true] = Expression.lex("a & 1 == true")

    assert ["a", :pipe, {:integer, 1}, :is_equal, true] =
             Expression.lex("a | 1 == true")

    assert ["a", :caret, {:integer, 1}, :is_equal, true] =
             Expression.lex("a ^ 1 == true")
  end

  test "handles logical operators" do
    assert ["a", :and, true] = Expression.lex("a and true")
    assert ["a", :and, :not, true] = Expression.lex("a and not true")
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
              [{:integer, 123}, :star, {:parens, [{:integer, 456}, :dash, {:integer, 234}]}]}
           ] = Expression.lex("a.method(123 * (456 - 234))")
  end

  test "handles arrows" do
    assert ["abc", :is_lt, {:integer, 123}] = Expression.lex("abc < 123")
    assert ["abc", :is_gt, {:integer, 123}] = Expression.lex("abc > 123")
    assert ["abc", :is_lt_or_equal, {:integer, 123}] = Expression.lex("abc <= 123")
    assert ["abc", :is_gt_or_equal, {:integer, 123}] = Expression.lex("abc >= 123")
  end

  test "handles brackets" do
    assert [{:brackets, [{:integer, 1}]}] = Expression.lex("[1]")

    assert [{:brackets, [{:integer, 1}, :comma, "abc", :comma, {:integer, 2}]}] =
             Expression.lex("[1, abc, 2]")
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

  test "handles exclamation of literals" do
    assert [:exclamation, false] = Expression.lex("!false")
    assert [:exclamation, true] = Expression.lex("!true")
    assert [:exclamation, :exclamation, false] = Expression.lex("!!false")
    assert [:exclamation, :exclamation, true] = Expression.lex("!!true")
    assert [:exclamation, :exclamation, :exclamation, false] = Expression.lex("!!!false")
    assert [:exclamation, :exclamation, :exclamation, true] = Expression.lex("!!!true")
  end
end

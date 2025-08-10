defmodule KaitaiToolkitTest.Expressions.ParseTest do
  use ExUnit.Case
  alias KaitaiToolkit.Ksy.Expression

  test "handles basic integers" do
    assert {:literal, 123} = parse_string!("123")
    assert {:negative, {:literal, 123}} = parse_string!("-123")
    assert {:literal, 204} = parse_string!("0xCC")
  end

  test "handles basic names" do
    assert {:name, "abc"} = parse_string!("abc")
    assert {:name, "some_longer_name"} = parse_string!("some_longer_name")
  end

  test "handles meta variables" do
    assert {:meta, :io} = parse_string!("_io")
    assert {:meta, :root} = parse_string!("_root")
    assert {:meta, :parent} = parse_string!("_parent")
  end

  test "handles ternary expressions" do
    assert {:ternary, {:name, "abc"}, {:literal, 123}, {:negative, {:literal, 456}}} =
             parse_string!("abc ? 123 : -456")
  end

  test "handles parenthesis" do
    assert {:parens, {:literal, 123}} = parse_string!("(123)")
    assert {:parens, {:negative, {:literal, float_val}}} = parse_string!("(-425.672e-1)")
    assert_in_delta(float_val, 42.5672, 0.01)
  end

  test "handles method calls" do
    assert {:method_call, {:name, "abc"}, {:name, "method"}, []} =
             parse_string!("abc.method()")

    assert {:method_call, {:name, "abc"}, {:name, "method"}, [{:name, "a"}, {:name, "b"}, {:name, "c"}]} =
             parse_string!("abc.method(a, b, c)")
  end

  test "handles basic math" do
    assert {:multiply, {:literal, 100}, {:parens, {:subtract, {:literal, 4}, {:literal, 2}}}} =
             parse_string!("100 * (4 - 2)")

    assert {:add, [{:name, "a"}, {:name, "b"}, {:name, "c"}]} =
             parse_string!("a + b + c")
  end

  test "handles arrays" do
    assert {:array,
            [
              literal: 0x50,
              literal: 0x41,
              literal: 0x43,
              literal: 0x4B,
              literal: 0x2D,
              literal: 0x31
            ]} = parse_string!("[0x50, 0x41, 0x43, 0x4b, 0x2d, 0x31]")
  end

  test "handles strings" do
    assert {:string, "Apple\nbottom\tjeans"} = parse_string!(~S|"Apple\nbottom\tjeans"|)
    assert {:add, [{:string, "Apple\n"}, {:string, "bottom\t"}, {:string, "jeans"}]} = parse_string!(~S|"Apple\n" + "bottom\t" + "jeans"|)
  end

  def parse_string!(str) do
    str
    |> Expression.lex()
    |> Expression.parse()
  end
end

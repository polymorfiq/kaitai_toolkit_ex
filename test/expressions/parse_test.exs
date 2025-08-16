defmodule KaitaiToolkitTest.Expressions.ParseTest do
  use ExUnit.Case
  alias KaitaiToolkit.Ksy.Expression

  test "handles basic integers" do
    assert 123 = parse_string!("123")
    assert -123 = parse_string!("-123")
    assert 204 = parse_string!("0xCC")
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
    assert {:ternary, {:name, "abc"}, 123, -456} =
             parse_string!("abc ? 123 : -456")
  end

  test "handles parenthesis" do
    assert 123 = parse_string!("(123)")
    assert float_val = parse_string!("(-425.672e-1)")
    assert_in_delta(float_val, -42.5672, 0.01)
  end

  test "handles method calls" do
    assert {:method_call, {:name, "abc"}, {:name, "method"}, []} =
             parse_string!("abc.method()")

    assert {:method_call, {:name, "abc"}, {:name, "method"},
            [{:name, "a"}, {:name, "b"}, {:name, "c"}]} =
             parse_string!("abc.method(a, b, c)")

    assert {:method_call, {:property, {:name, "abc"}, {:name, "property"}}, {:name, "method"},
            [{:name, "a"}, {:name, "b"}, {:name, "c"}]} =
             parse_string!("abc.property.method(a, b, c)")
  end

  test "handles basic math" do
    assert 10_000 = parse_string!("100 * 100")
    assert 0x14 = parse_string!("0xA * 0x2")
    assert 6 = parse_string!("0b11 * 0x2")
    assert 12.5 = parse_string!("5.0 * 2.5")
    assert 10.0 = parse_string!("5.0 * 2")

    assert 40 = parse_string!("1000 / 25")
    assert 40 = parse_string!("1_000 / 2_5")
    assert 0x5 = parse_string!("0xA / 0x2")
    assert 3 = parse_string!("0b110 / 0x2")
    assert 2.0 = parse_string!("5.0 / 2.5")
    assert 2.5 = parse_string!("5.0 / 2")

    assert -25 = parse_string!("-25")
    assert 975 = parse_string!("1000 - 25")
    assert 975 = parse_string!("1_000 - 2_5")
    assert -1500 = parse_string!("1_000 - 2_500")
    assert 0x8 = parse_string!("0xA - 0x2")
    assert 4 = parse_string!("0b110 - 0x2")
    assert 2.5 = parse_string!("5.0 - 2.5")
    assert 3.0 = parse_string!("5.0 - 2")
    assert -3.0 = parse_string!("2 - 5.0")

    assert 1025 = parse_string!("1000 + 25")
    assert 1025 = parse_string!("1_000 + 2_5")
    assert 3500 = parse_string!("1_000 + 2_500")
    assert 0xC = parse_string!("0xA + 0x2")
    assert 8 = parse_string!("0b110 + 0x2")
    assert 7.5 = parse_string!("5.0 + 2.5")
    assert 7.0 = parse_string!("5.0 + 2")
    assert 7.0 = parse_string!("2 + 5.0")

    assert 6 = parse_string!("5 * 2 - 4")
    assert -18 = parse_string!("2 - 4 * 5")
    assert 1.2 = parse_string!("2 - 4 / 5.0")
    assert 0.1 = parse_string!("2 / 4 * 5.0")
    assert 2.5 = parse_string!("(2.0 / 4.0) * 5.0")
    assert 8.0 = parse_string!("(2.0 / 4.0) * (5.0 * 2) + 3")

    assert 200 = parse_string!("100 * (4 - 2)")
  end

  test "handles variable math" do
    assert {:add, {:add, {:name, "a"}, {:name, "b"}}, {:name, "c"}} =
             parse_string!("a + b + c")

    assert {:multiply, {:multiply, 1, {:name, "b"}}, 5} =
             parse_string!("1 * b * 5")
  end

  test "handles arrays" do
    assert [0x50, 0x41, 0x43, 0x4B, 0x2D, 0x31] = parse_string!("[0x50, 0x41, 0x43, 0x4b, 0x2d, 0x31]")
  end

  test "handles strings" do
    assert {:string, "Apple\nbottom\tjeans"} = parse_string!(~S|"Apple\nbottom\tjeans"|)
    assert {:string, "Apple\nbottom\tjeans"} = parse_string!(~S|"Apple\n" + "bottom\t" + "jeans"|)
  end

  test "handles boolean logic" do
    assert true == parse_string!(~S|true|)
    assert false == parse_string!(~S|false|)
    assert true == parse_string!(~S|false or true|)
    assert false == parse_string!(~S|false and true|)
    assert false == parse_string!(~S|false or true and false|)
  end

  test "handles comparisons" do
    assert true == parse_string!(~S|100 == 100|)
    assert false == parse_string!(~S|103 == 100|)
    assert false == parse_string!(~S|100 != 100|)
    assert true == parse_string!(~S|103 != 100|)
    assert true == parse_string!(~S|5 > 4|)
    assert true == parse_string!(~S|5.1 > 5|)
    assert false == parse_string!(~S|3 > 4|)
    assert true == parse_string!(~S|5 > 4|)
    assert false == parse_string!(~S|3 > 4|)
    assert true == parse_string!(~S|104 <= 104|)
    assert false == parse_string!(~S|105 <= 104|)
  end

  test "handle bitwise calculations" do
    assert 16 == parse_string!(~S|2 << 3|)
    assert true == parse_string!(~S|2 << 3 > 15|)
    assert false == parse_string!(~S|2 << 3 < 16|)
    assert true == parse_string!(~S|2 << 3 == 16 and 2 >> 3 == 0|)
    assert true == parse_string!(~S|1 & 1 == 1|)
    assert true == parse_string!(~S|1 & 0 == 0|)
    assert true == parse_string!("1 | 1 == 1")
    assert true == parse_string!("0 | 1 == 1")
    assert true == parse_string!("0 | 0 == 0")
  end

  test "handles more complex comparisons" do
    assert true == parse_string!(~S|100 == 100 and 200 != 200 or 100 < 200|)
    assert false == parse_string!(~S|100 == 100 and 200 != 200 or 100 > 200|)
    assert true == parse_string!(~S|3 > 2 and 2 < 3|)
    assert true == parse_string!(~S|3 > 2 != 4 < 3|)
  end

  def parse_string!(str) do
    str
    |> Expression.lex()
    |> Expression.parse()
  end
end

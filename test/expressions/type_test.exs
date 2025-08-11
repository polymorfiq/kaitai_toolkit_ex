defmodule KaitaiToolkitTest.Expressions.TypeTest do
  use ExUnit.Case

  alias KaitaiToolkit.Ksy.Expression
  alias KaitaiToolkit.Ksy.TypeSystem

  test "handles strings correctly" do
    assert :string = TypeSystem.type!({:string, "some_string"})
    assert :string = type_string!(~S|"a string"|)
    assert :string = type_string!(~S|("a string")|)
    assert :string = type_string!(~S|("a") + "b" + ("c")|)
  end

  test "handles integers correctly" do
    assert :integer = TypeSystem.type!({:literal, 123})
    assert :integer = type_string!("1 + 2")
    assert :integer = type_string!("3 * 52")
    assert :integer = type_string!("3 * 52 / 2")
  end

  test "handles floats correctly" do
    assert :float = TypeSystem.type!({:literal, 123.2})
    assert :float = type_string!("1.0 + 2.0")
    assert :float = type_string!("1.0 + 2")
    assert :float = type_string!("1 + 2.0")
    assert :float = type_string!("3 * 52.0")
    assert :float = type_string!("3 * 52 / 2.0")
  end

  def type_string!(str) do
    str
    |> Expression.lex()
    |> Expression.parse()
    |> TypeSystem.type!()
  end
end
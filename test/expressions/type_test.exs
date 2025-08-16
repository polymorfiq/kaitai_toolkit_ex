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
    assert :integer = TypeSystem.type!(123)
    assert :integer = type_string!("1 + 2")
    assert :integer = type_string!("3 * 52")
    assert :integer = type_string!("3 * 52 / 2")
  end

  test "handles floats correctly" do
    assert :float = TypeSystem.type!(123.2)
    assert :float = type_string!("1.0 + 2.0")
    assert :float = type_string!("1.0 + 2")
    assert :float = type_string!("1 + 2.0")
    assert :float = type_string!("3 * 52.0")
    assert :float = type_string!("3 * 52 / 2.0")
  end

  test "handles arrays of integers correctly" do
    assert {:array, :integer} = TypeSystem.type!([123, 456, -789])

    assert {:array, :integer} =
             type_string!("[0x41_AC, 0b1101, 3242252543666435675636554242534636]")
  end

  test "handles arrays of floats correctly" do
    assert {:array, :float} = TypeSystem.type!([123.0, 456.2, -789.3])
    assert {:array, :float} = type_string!("[3.2, 15.3e-3, 1_5._3e-3, 523_123.24_245]")
  end

  test "handles arrays of strings correctly" do
    assert {:array, :string} = TypeSystem.type!([{:string, "a"}, {:string, "b"}, {:string, "c"}])
    assert {:array, :string} = type_string!(~S|["d", "ef", "g"]|)
    assert {:array, :string} = type_string!(~S|([("d"), "ef", "g"])|)
  end

  test "handles add correctly" do
    assert :integer = TypeSystem.type!({:add, 3, 2})
    assert :float = TypeSystem.type!({:add, 3, 2.0})
    assert :float = TypeSystem.type!({:add, 3.0, 2})
    assert :float = TypeSystem.type!({:add, 3.0, 2.0})
    assert :string = TypeSystem.type!({:add, {:string, "a"}, {:string, "b"}})

    assert :integer = type_string!("1 + 3 + (5)")
    assert :float = type_string!("1 + 2.0 + 2")
    assert :float = type_string!("1.0 + 2.0")
    assert :float = type_string!("1.0 + 2")
    assert {:error, "Non-homogenous add" <> _} = type_string(~S|1 + "something"|)
    assert {:error, "Non-homogenous add" <> _} = type_string(~S|"something" + 1.0|)
  end

  test "handles subtract correctly" do
    assert :integer = TypeSystem.type!({:subtract, 3, 2})
    assert :float = TypeSystem.type!({:subtract, 3, 2.0})
    assert :float = TypeSystem.type!({:subtract, 3.0, 2})
    assert :float = TypeSystem.type!({:subtract, 3.0, 2.0})

    assert :integer = type_string!("1 - 3 - (5)")
    assert :float = type_string!("1 - 2.0 - 2")
    assert :float = type_string!("1.0 - 2.0")
    assert :float = type_string!("1.0 - 2")
    assert {:error, "Non-homogenous subtract" <> _} = type_string(~S|1 - "something"|)
    assert {:error, "Non-homogenous subtract" <> _} = type_string(~S|"something" - 1.0|)
  end

  test "handles multiply correctly" do
    assert :integer = TypeSystem.type!({:multiply, 3, 2})
    assert :float = TypeSystem.type!({:multiply, 3, 2.0})
    assert :float = TypeSystem.type!({:multiply, 3.0, 2})
    assert :float = TypeSystem.type!({:multiply, 3.0, 2.0})

    assert :integer = type_string!("1 * 3 * (5)")
    assert :float = type_string!("1 * 2.0 * 2")
    assert :float = type_string!("1.0 * 2.0")
    assert :float = type_string!("1.0 * 2")
    assert {:error, "Non-homogenous multiply" <> _} = type_string(~S|1 * "something"|)
    assert {:error, "Non-homogenous multiply" <> _} = type_string(~S|"something" * 1.0|)
  end

  test "handles divide correctly" do
    assert :integer = TypeSystem.type!({:divide, 3, 2})
    assert :float = TypeSystem.type!({:divide, 3, 2.0})
    assert :float = TypeSystem.type!({:divide, 3.0, 2})
    assert :float = TypeSystem.type!({:divide, 3.0, 2.0})

    assert :integer = type_string!("1 / 3")
    assert :float = type_string!("1/2.0")
    assert :float = type_string!("1.0 / 2.0")
    assert :float = type_string!("1.0 / 2")
    assert {:error, "Non-homogenous divide" <> _} = type_string(~S|1 / "something"|)
    assert {:error, "Non-homogenous divide" <> _} = type_string(~S|"something" / 1.0|)
  end

  test "handles mixed math correctly" do
    assert :integer =
             TypeSystem.type!({:multiply, 3, {:add, 3, 2}})

    assert :float =
             TypeSystem.type!({:multiply, 3, {:add, 3.0, 2}})

    assert :float =
             TypeSystem.type!({:multiply, 3.0, {:add, 3, 2}})

    assert :float =
             TypeSystem.type!(
               {:multiply, 3.0, {:add, 3, 2.0}}
             )

    assert :integer = type_string!("1 / 3*3")
    assert :float = type_string!("2*1/2.0")
    assert :float = type_string!("1.0 / 2.0 + 3")
    assert :float = type_string!("1.0 / 2 + -3")
  end

  def type_string!(str) do
    str
    |> Expression.lex()
    |> Expression.parse()
    |> TypeSystem.type!()
  end

  def type_string(str) do
    str
    |> Expression.lex()
    |> Expression.parse()
    |> TypeSystem.type()
  end
end

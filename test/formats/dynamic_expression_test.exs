defmodule KaitaiToolkitTest.Formats.DynamicEpressionTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can handle repeat-eos" do
    defmodule ListOfThings do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: things
              type: s4
              repeat: eos
        """
    end

    io =
      binary_stream(<<
        24::signed-integer-32,
        102::signed-integer-32,
        42::signed-integer-32,
        24::signed-integer-32,
        -523::signed-integer-32,
        523::signed-integer-32,
        -1024::signed-integer-32
      >>)

    list_of_things = ListOfThings.read!(io)
    assert [24, 102, 42, 24, -523, 523, -1024] = list_of_things.things
  end

  test "can handle integer repeat-expr" do
    defmodule IntegerRepeatExpr do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: things
              type: s4
              repeat: expr
              repeat-expr: 5
        """
    end

    io =
      binary_stream(<<
        24::signed-integer-32,
        102::signed-integer-32,
        42::signed-integer-32,
        24::signed-integer-32,
        -523::signed-integer-32
      >>)

    list_of_things = IntegerRepeatExpr.read!(io)
    assert [24, 102, 42, 24, -523] = list_of_things.things
  end

  test "can handle constant expression repeat-expr" do
    defmodule StaticRepeatExpr do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: things
              type: s4
              repeat: expr
              repeat-expr: 3 * 2
        """
    end

    io =
      binary_stream(<<
        24::signed-integer-32,
        102::signed-integer-32,
        42::signed-integer-32,
        24::signed-integer-32,
        -523::signed-integer-32,
        5631::signed-integer-32
      >>)

    list_of_things = StaticRepeatExpr.read!(io)
    assert [24, 102, 42, 24, -523, 5631] = list_of_things.things
  end

  test "can handle runtime-dependent repeat-expr" do
    defmodule RuntimeDependentRepeat do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: num_of_things
              type: u4
            - id: things
              type: s2
              repeat: expr
              repeat-expr: num_of_things * 2
        """
    end

    io =
      binary_stream(<<
        # num_of_things
        2::unsigned-integer-32,
        102::signed-integer-16,
        633::signed-integer-16,
        -345::signed-integer-16,
        71::signed-integer-16
      >>)

    list_of_things = RuntimeDependentRepeat.read!(io)
    assert 2 = list_of_things.num_of_things
    assert [102, 633, -345, 71] = list_of_things.things
  end

  test "can handle more complicated runtime-dependent repeat-expr" do
    defmodule ComplicatedRuntimeDependentRepeat do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: num_of_things
              type: u4
            - id: num_of_things_multiplier
              type: u4
            - id: things
              type: s2
              repeat: expr
              repeat-expr: num_of_things * (num_of_things_multiplier + 1)
        """
    end

    io =
      binary_stream(<<
        # num_of_things
        2::unsigned-integer-32,
        # num_of_things_multiplier
        1::unsigned-integer-32,
        102::signed-integer-16,
        633::signed-integer-16,
        -345::signed-integer-16,
        71::signed-integer-16
      >>)

    list_of_things = ComplicatedRuntimeDependentRepeat.read!(io)
    assert 2 = list_of_things.num_of_things
    assert 1 = list_of_things.num_of_things_multiplier
    assert [102, 633, -345, 71] = list_of_things.things
  end

  test "can handle basic repeat-until" do
    defmodule RuntimeRepeatUntil do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: things
              type: s2
              repeat: until
              repeat-until: _ == 7234
        """
    end

    io =
      binary_stream(<<
        102::signed-integer-16,
        634::signed-integer-16,
        7234::signed-integer-16,
        522::signed-integer-16
      >>)

    list_of_things = RuntimeRepeatUntil.read!(io)
    assert [102, 634, 7234] = list_of_things.things
  end

  test "can handle basic string to integer" do
    defmodule RuntimeStringToInteger do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: list_of_things
            title: A bunch of things
          seq:
            - id: things
              type: s2
              repeat: until
              repeat-until: "\\"AD\\".to_i(16).to_s() == \\"173\\""
        """
    end

    io =
      binary_stream(<<
        102::signed-integer-16,
        634::signed-integer-16,
        7234::signed-integer-16,
        522::signed-integer-16
      >>)

    list_of_things = RuntimeStringToInteger.read!(io)
    assert [102] = list_of_things.things
  end

  test "can handle a parent call" do
    defmodule RuntimeParentExpression do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_things
              title: A bunch of things
            seq:
              - id: num_things
                type: s2
              - id: thing
                type: a_thing
            types:
              a_thing:
                seq:
                  - id: things
                    type: s2
                    repeat: expr
                    repeat-expr: _parent.num_things
          """
    end

    io =
      binary_stream(<<
        3::signed-integer-16,
        1::signed-integer-16,
        2::signed-integer-16,
        3::signed-integer-16
      >>)

    list_of_things = RuntimeParentExpression.read!(io)
    assert 3 == list_of_things.num_things
    assert [1, 2, 3] = list_of_things.thing.things
  end

  test "can handle a nested parent call" do
    defmodule RuntimeNestedParentExpression do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_things
              title: A bunch of things
            seq:
              - id: num_things
                type: s2
              - id: num_double_things
                type: s2
              - id: thing
                type: a_thing
            types:
              a_thing:
                seq:
                  - id: single_things
                    type: s2
                    repeat: expr
                    repeat-expr: _parent.num_things
                  - id: double
                    type: double_nested
              double_nested:
                seq:
                  - id: single_double_things
                    type: s2
                    repeat: expr
                    repeat-expr: _parent.single_things.length
                  - id: double_things
                    type: s2
                    repeat: expr
                    repeat-expr: _root.num_double_things
          """
    end

    io =
      binary_stream(<<
        1::signed-integer-16,
        3::signed-integer-16,
        1::signed-integer-16,
        2::signed-integer-16,
        3::signed-integer-16,
        4::signed-integer-16,
        5::signed-integer-16,
      >>)

    list_of_things = RuntimeNestedParentExpression.read!(io)
    assert 1 == list_of_things.num_things
    assert 3 == list_of_things.num_double_things
    assert [1] == list_of_things.thing.single_things
    assert [2] == list_of_things.thing.double.single_double_things
    assert [3, 4, 5] == list_of_things.thing.double.double_things
  end


  test "can handle a stream ops" do
    defmodule RuntimeStreamOps do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_things
              title: A bunch of things
            seq:
              - id: half_the_things
                size: _io.size / 2
              - id: other_half_the_things
                size: _io.size / 2
          """
    end

    io =
      binary_stream(<<
        1::signed-integer-8,
        2::signed-integer-8,
        3::signed-integer-8,
        4::signed-integer-8,
      >>)

    list_of_things = RuntimeStreamOps.read!(io)
    assert <<1, 2>> == list_of_things.half_the_things
    assert <<3, 4>> == list_of_things.other_half_the_things
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end

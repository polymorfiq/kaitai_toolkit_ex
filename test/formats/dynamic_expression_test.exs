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
        5631::signed-integer-32,
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
        2::unsigned-integer-32, # num_of_things
        102::signed-integer-16,
        633::signed-integer-16,
        -345::signed-integer-16,
        71::signed-integer-16,
      >>)

    list_of_things = RuntimeDependentRepeat.read!(io)
    assert 2 = list_of_things.num_of_things
    assert [102, 633, -345, 71] = list_of_things.things
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end

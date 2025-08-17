defmodule KaitaiToolkitTest.Formats.ValidationTest do
  use ExUnit.Case

  test "will accept valid contents" do
    defmodule ContentsValidationChecker do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_bytes
            seq:
              - id: a_thing
                size: 1
              - id: checked_contents
                contents: [0xC1, 0x83, 0x2A, 0x9E]
              - id: b_thing
                size: 1
          """
    end

    io =
      binary_stream(<<
        0x1::integer-8,
        0xC1::integer-8,
        0x83::integer-8,
        0x2A::integer-8,
        0x9E::integer-8,
        0x2::integer-8
      >>)

    packet = ContentsValidationChecker.read!(io)
    assert <<0x1::integer-8>> = packet.a_thing
    assert <<
             0xC1::integer-8,
             0x83::integer-8,
             0x2A::integer-8,
             0x9E::integer-8
           >> = packet.checked_contents
    assert <<0x2::integer-8>> = packet.b_thing
  end

  test "will raise on invalid contents" do
    defmodule ContentsRejection do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_bytes
            seq:
              - id: a_thing
                size: 1
              - id: checked_contents
                contents: [0xC1, 0x83, 0x2A, 0x9E]
              - id: b_thing
                size: 1
          """
    end

    io =
      binary_stream(<<
        0x1::integer-8,
        0xC1::integer-8,
        0x83::integer-8,
        0x2D::integer-8,
        0x9E::integer-8,
        0x2::integer-8
      >>)

    assert_raise KaitaiStruct.Stream.ReadError, fn ->
      ContentsRejection.read!(io)
    end
  end

  test "allows string-based contents checks" do
    defmodule AsciiCheck do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_bytes
            seq:
              - id: a_thing
                size: 1
              - id: checked_contents
                contents: abc
              - id: b_thing
                size: 1
          """
    end

    io =
      binary_stream(<<0x1, 97, 98, 99, 0x2>>)

    ascii_check = AsciiCheck.read!(io)
    assert <<0x1::integer-8>> = ascii_check.a_thing
    assert "abc" = ascii_check.checked_contents
    assert <<0x2::integer-8>> = ascii_check.b_thing
  end

  test "allows array of items for contents" do
    defmodule ArrayContentsCheck do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_bytes
            seq:
              - id: checked_contents
                contents: [1, abc, 2, d]
          """
    end

    io =
      binary_stream(<<0x1, 97, 98, 99, 0x2, 100>>)

    ascii_check = ArrayContentsCheck.read!(io)
    assert <<1, "abc"::binary, 2, "d"::binary>> = ascii_check.checked_contents
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end

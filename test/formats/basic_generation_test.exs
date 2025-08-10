defmodule KaitaiToolkitTest.Formats.BasicGenerationTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can read a basic spec without crashing" do
    defmodule TestExample do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: basic_save_file
            title: Example save file
          seq:
            - id: header
              type: save_file_header
            - id: compressed_data
              type: compressed_save_file_body
              repeat: eos
          types:
            save_file_header:
              seq:
                - id: save_header_version
                  type: s4le
                - id: data_stream
                  type: io
        """
    end

    io = binary_stream(<<14::size(128)>>)
    assert TestExample.SaveFileHeader.read!(io)
  end

  test "parses basic structures correctly" do
    defmodule BasicStructure do
      use KaitaiToolkit.Struct,
        contents: """
          meta:
            id: some_structure
          seq:
            - id: a_count
              type: u1
            - id: b_count
              type: s2
            - id: c_count
              type: u4
            - id: d_count
              type: f4
        """
    end

    io =
      binary_stream(
        <<24::unsigned-integer-8, -48::signed-integer-16, 96::unsigned-integer-32,
          192.5::float-32>>
      )

    structure = BasicStructure.read!(io)

    assert structure.a_count == 24
    assert structure.b_count == -48
    assert structure.c_count == 96
    assert_in_delta(structure.d_count, 192.5, 0.1)
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end

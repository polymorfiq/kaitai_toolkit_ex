defmodule KaitaiToolkitTest.Formats.DynamicSizesTest do
  use ExUnit.Case

  test "can read a full-stream byte-array" do
    defmodule FullStreamByteArray do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_bytes
            seq:
              - id: a_thing
                size: 1
              - id: the_list
                size-eos: true
          """
    end

    io =
      binary_stream(<<24::unsigned-integer-8, -5::signed-integer-32, 32::signed-integer-32, 104::signed-integer-32>>)

    packet = FullStreamByteArray.read!(io)
    assert <<24::unsigned-integer-8>> = packet.a_thing
    assert <<-5::signed-integer-32, 32::signed-integer-32, 104::signed-integer-32>> = packet.the_list
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end
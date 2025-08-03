defmodule KaitaiToolkitTest.Formats.DnsPacketTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can read a basic spec without crashing" do
    defmodule DnsPacket do
      use KaitaiToolkit.Struct, ksy: "#{:code.priv_dir(:kaitai_toolkit)}/dns_packet.ksy"
    end

    io = binary_stream(<<
      123::unsigned-integer-16, # Transaction ID
      0::unsigned-integer-16, # Packet Flags
      0::unsigned-integer-16, # qdcount
      0::unsigned-integer-16, # ancount
      0::unsigned-integer-16, # nscount
      0::unsigned-integer-16, # arcount
    >>)

    packet = DnsPacket.read!(io)
    assert packet.transaction_id == 123
    assert packet.flags == 0
    assert packet.qdcount == 0
    assert packet.ancount == 0
    assert packet.nscount == 0
    assert packet.arcount == 0
  end

  defp binary_stream(bin_data) do
    io_stream = StringIO.open(bin_data) |> then(fn {:ok, io} -> IO.binstream(io, 1) end)
    {:ok, kaitai} = GenServer.start_link(KaitaiStruct.Stream, {io_stream, byte_size(bin_data)})
    kaitai
  end
end

defmodule KaitaiToolkitTest.Formats.DnsPacketTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can read a basic spec without crashing" do
    defmodule DnsPacket do
      use KaitaiToolkit.Struct, ksy: "#{:code.priv_dir(:kaitai_toolkit)}/dns_packet.ksy"
    end

    io =
      binary_stream(<<
        # Transaction ID
        123::unsigned-integer-16,
        # Packet Flags
        0::unsigned-integer-16,
        # qdcount
        0::unsigned-integer-16,
        # ancount
        0::unsigned-integer-16,
        # nscount
        0::unsigned-integer-16,
        # arcount
        0::unsigned-integer-16
      >>)

    packet = DnsPacket.read!(io)
    assert packet.transaction_id == 123
    assert packet.flags.flag == 0
    assert packet.qdcount == 0
    assert packet.ancount == 0
    assert packet.nscount == 0
    assert packet.arcount == 0
  end

  test "can read an CNAME record" do
    defmodule CnameDnsPacket do
      use KaitaiToolkit.Struct, ksy: "#{:code.priv_dir(:kaitai_toolkit)}/dns_packet.ksy"
    end

    io =
      binary_stream(<<
        # Transaction ID
        123::unsigned-integer-16,
        # Packet Flags
        0::unsigned-integer-16,
        # qdcount
        0::unsigned-integer-16,
        # ancount
        1::unsigned-integer-16,
        # nscount
        0::unsigned-integer-16,
        # arcount
        0::unsigned-integer-16,
        # Label Length
        4::unsigned-integer-8,
        # Label
        "test"::binary,
        # Label Length
        3::unsigned-integer-8,
        # Label
        "com"::binary,
        # No more labels
        0::unsigned-integer-8,
        # CNAME
        5::unsigned-integer-big-16,
        # Internet address
        <<0x00, 0x01>>::binary,
        # TTL
        300::unsigned-integer-32,
        # RD Length
        0::unsigned-integer-16,
      >>)

    packet = CnameDnsPacket.read!(io)
    assert packet.transaction_id == 123
    assert packet.flags.flag == 0
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

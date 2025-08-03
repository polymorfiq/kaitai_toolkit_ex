defmodule KaitaiToolkitTest.Formats.DnsPacketTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can read a basic spec without crashing" do
    defmodule DnsPacket do
      use KaitaiToolkit.Struct, ksy: "#{:code.priv_dir(:kaitai_toolkit)}/dns_packet.ksy"
    end

    assert DnsPacket.read!(123)
  end
end

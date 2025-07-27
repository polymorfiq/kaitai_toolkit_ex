defmodule KaitaiToolkitTest.Formats.BasicGenerationTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "can read a basic spec without crashing" do
    defmodule TestExample do
      use KaitaiToolkit.Struct, contents: """
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
      """
    end

    assert TestExample.SaveFileHeader.read!(123)
  end
end

defmodule KaitaiToolkitTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "has a use_kaitai_struct macro" do
    defmodule TestExample do
      import KaitaiToolkit
      use_kaitai_struct """
      abc
      """
    end
  end
end

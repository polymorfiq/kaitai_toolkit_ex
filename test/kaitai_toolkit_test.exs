defmodule KaitaiToolkitTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "has a use macro" do
    defmodule TestExample do
      use KaitaiToolkit.Struct, contents: ""
    end
  end
end

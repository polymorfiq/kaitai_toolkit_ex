defmodule KaitaiToolkitTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "greets the world" do
    assert KaitaiToolkit.hello() == :world
  end
end

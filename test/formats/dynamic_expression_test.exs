defmodule KaitaiToolkitTest.Formats.DynamicEpressionTest do
  use ExUnit.Case
  doctest KaitaiToolkit

  test "infers the type of a basic math expression" do
    defmodule TestExample do
      use KaitaiToolkit.Struct,
          contents: """
            meta:
              id: list_of_things
              title: A bunch of things
            seq:
              - id: things
                type: s4
                repeat: expr
                repeat-expr: 4 * 3
          """
    end

    # ... put an actual test here ...
  end
end

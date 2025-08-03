# Kaitai Elixir Toolkit

**Note:**
This is **Work in Progress** and **should not** be considered a Production-ready library.

[Kaitai Struct](https://kaitai.io/) is a declarative language to describe binary data structures that can be transpiled into multiple target languages.

Kaitai Elixir Toolkit is a library for directly interpreting the Kaitai Struct language for common tasks. The toolkit **is not** the officially recommended process for transpiling Kaitai into Elixir, which would be using the [official compiler](https://github.com/kaitai-io/kaitai_struct_compiler) to generate code in the target language.

This is instead a passion project by someone who loves Elixir and Code Generation and would like to combine the two. 

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `kaitai_toolkit` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:kaitai_toolkit, "~> 0.0.3"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/kaitai_toolkit>.


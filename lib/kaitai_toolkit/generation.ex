defmodule KaitaiToolkit.Generation do
  alias KaitaiToolkit.Generation.Attribute
  alias KaitaiToolkit.Generation.Module, as: KaitaiModule

  defstruct [:attrs, :modules]

  def generate(ksy, opts \\ []) do
    root_module = type_to_module(ksy)

    generated = %__MODULE__{
      modules: Map.new(extract_modules(ksy, opts))
    }

    solved_deps = solve_module_deps_order(generated.modules)

    nested_modules =
      solved_deps
      |> Enum.map(fn {mod_name, mod} ->
        gen_module(mod_name, mod, opts)
      end)

    quote do
      unquote_splicing(gen_aliases(root_module))

      unquote_splicing(nested_modules)

      unquote(gen_module_defstruct(root_module))
      unquote(gen_module_typespec(root_module))

      unquote_splicing(gen_read_functions(root_module, opts))
    end
  end

  defp gen_module(mod_name, mod, opts) do
    quote do
      defmodule unquote(mod_name) do
        unquote_splicing(gen_aliases(mod))

        unquote(gen_module_defstruct(mod))
        unquote(gen_module_typespec(mod))

        unquote_splicing(gen_read_functions(mod, opts))
      end
    end
  end

  defp gen_module_defstruct(mod) do
    quote do
      defstruct unquote(mod.attrs |> Enum.map(&String.to_atom(&1.name)))
    end
  end

  defp gen_module_typespec(mod) do
    type_spec = {:%{}, [], Enum.map(mod.attrs, &{String.to_atom(&1.name), &1.elixir_type})}
    struct_spec = {:%, [], [{:__MODULE__, [], Elixir}, type_spec]}

    quote do
      @type t :: unquote(struct_spec)
    end
  end

  defp gen_aliases(_mod) do
    [
      quote do
        alias KaitaiStruct.Stream, as: KaitaiStream
      end
    ]
  end

  defp gen_read_functions(mod, opts) do
    read_spec = quote do
      @spec(read!(io :: KaitaiStream.t()) :: t())
    end

    base_map = quote do: %{}
    read_def =
      quote do
        def read!(io) do
          unquote(gen_read_steps(mod, base_map, mod.attrs, opts))
        end
      end

    [read_spec, read_def]
  end

  defp gen_read_steps(_mod, left, [], _opts) do
    quote do
      unquote(left) |> then(& struct(__MODULE__, &1))
    end
  end

  defp gen_read_steps(mod, left, [attr | rest_attrs], opts) do
    _attr_id = String.to_atom(attr.name)
    with_step = gen_read_step(attr, mod, left, opts)

    gen_read_steps(mod, with_step, rest_attrs, opts)
  end

  defp gen_read_step(%{data_type: :u1} = attr, _mod, left, _opts),
    do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u1!(io))

  defp gen_read_step(%{data_type: :u2} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u2be!(io))

  defp gen_read_step(%{data_type: :u2le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u2le!(io))

  defp gen_read_step(%{data_type: :u2be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u2be!(io))

  defp gen_read_step(%{data_type: :u4} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u4be!(io))

  defp gen_read_step(%{data_type: :u4le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u4le!(io))

  defp gen_read_step(%{data_type: :u4be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u4be!(io))

  defp gen_read_step(%{data_type: :u8} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u8be!(io))

  defp gen_read_step(%{data_type: :u8le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u8le!(io))

  defp gen_read_step(%{data_type: :u8be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_u8be!(io))

  defp gen_read_step(%{data_type: :s1} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s1!(io))

  defp gen_read_step(%{data_type: :s2} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s2be!(io))

  defp gen_read_step(%{data_type: :s2le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s2le!(io))

  defp gen_read_step(%{data_type: :s2be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s2be!(io))

  defp gen_read_step(%{data_type: :s4} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s4be!(io))

  defp gen_read_step(%{data_type: :s4le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s4le!(io))

  defp gen_read_step(%{data_type: :s4be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s4be!(io))

  defp gen_read_step(%{data_type: :s8} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s8be!(io))

  defp gen_read_step(%{data_type: :s8le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s8le!(io))

  defp gen_read_step(%{data_type: :s8be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_s8be!(io))

  defp gen_read_step(%{data_type: :f4} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f4be!(io))

  defp gen_read_step(%{data_type: :f4le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f4le!(io))

  defp gen_read_step(%{data_type: :f4be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f4be!(io))

  defp gen_read_step(%{data_type: :f8} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f8be!(io))

  defp gen_read_step(%{data_type: :f8le} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f8le!(io))

  defp gen_read_step(%{data_type: :f8be} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_f8be!(io))

  defp gen_read_step(%{data_type: :bytes} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> then(& Map.put(&1, unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_bytes_array!(io, KaitaiToolkit.Struct.parse_expr!(&1, unquote(attr.attr.size)))))

  defp gen_read_step(%{data_type: :io} = attr, _mod, left, _opts),
       do: quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), io)

  defp gen_read_step(%{data_type: :str} = attr, _mod, left, _opts) do
    quote do
      unquote(left) |> then(& Map.put(&1, unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_bytes_array!(io, KaitaiToolkit.Struct.parse_expr!(&1, unquote(attr.attr.size)))))
    end
  end

  defp gen_read_step(%{data_type: :strz} = attr, _mod, left, _opts) do
    quote do
      unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), KaitaiStruct.Stream.read_bytes_term!(io, unquote(attr.attr.encoding || "UTF-8"), unquote(0), unquote(attr.attr.include), unquote(attr.attr.consume), unquote(attr.attr.eos_error)))
    end
  end

  defp gen_read_step(%{data_type: {:user_defined, name}} = attr, _mod, left, opts) do
    custom_mod_name = :"#{Module.concat(opts[:root], Macro.camelize(name))}"
    quote do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), unquote(custom_mod_name).read!(io))
  end

  defp gen_read_step(%{data_type: %{switch_on: _, cases: _}}, _mod, left, _opts) do
    quote do: unquote(left) |> then(fn _ -> raise "switches are not implemented yet!" end)
  end

  defp extract_modules(ksy, opts) do
    modules =
      ksy.types
      |> Enum.map(fn {type_id, type} ->
        mod_name = :"#{Module.concat(opts[:root], Macro.camelize(type_id))}"
        mod_def = type_to_module(type, opts)

        {mod_name, mod_def}
      end)

    nested_modules =
      ksy.types
      |> Enum.flat_map(fn {type_id, type} ->
        root = Module.concat(opts[:root], Macro.camelize(type_id))
        processed = extract_modules(type, opts ++ [root: root])

        processed
      end)

    modules ++ nested_modules
  end

  def type_to_module(type, opts \\ []) do
    type_root =
      case "#{Keyword.get(opts, :root)}" do
        "" -> []
        "Elixir." <> root -> String.split("#{root}", ".")
        root -> String.split("#{root}", ".")
      end

    deps =
      Enum.flat_map(type.seq, fn attr ->
        case attr.type do
          {:user_defined, name} ->
            [:"#{Module.concat(opts[:root], Macro.camelize(name))}"]

          _ ->
            []
        end
      end)

    attrs =
      type.seq
      |> Enum.map(fn attr ->
        %Attribute{
          name: attr.id,
          elixir_type: data_type_to_elixir(attr.type, type_root),
          data_type: attr.type,
          attr: attr
        }
      end)

    %KaitaiModule{attrs: attrs, deps: deps}
  end

  defp solve_module_deps_order(mods, solved \\ [])
  defp solve_module_deps_order([], solved), do: solved

  defp solve_module_deps_order(mods, solved) do
    {newly_solved, still_unsolved} =
      Enum.split_with(mods, fn {_, mod} ->
        solved_deps =
          mod.deps
          |> Enum.filter(&Enum.find(solved, fn {solved_name, _} -> solved_name == &1 end))

        Enum.count(solved_deps) == Enum.count(mod.deps)
      end)

    if Enum.count(newly_solved) == 0 && Enum.count(still_unsolved) > 0 do
      deps_left = Enum.map(still_unsolved, fn {mod_name, mod} -> {mod_name, mod.deps} end)
      raise "Cyclical dependencies? #{inspect(deps_left)}"
    end

    solve_module_deps_order(still_unsolved, solved ++ newly_solved)
  end

  defp data_type_to_elixir(:u1, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u2, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u2le, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u2be, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u4, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u4le, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u4be, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u8, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u8le, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:u8be, _), do: {:non_neg_integer, [], []}
  defp data_type_to_elixir(:s1, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s2, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s2le, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s2be, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s4, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s4le, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s4be, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s8, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s8le, _), do: {:integer, [], []}
  defp data_type_to_elixir(:s8be, _), do: {:integer, [], []}
  defp data_type_to_elixir(:f4, _), do: {:float, [], []}
  defp data_type_to_elixir(:f4le, _), do: {:float, [], []}
  defp data_type_to_elixir(:f4be, _), do: {:float, [], []}
  defp data_type_to_elixir(:f8, _), do: {:float, [], []}
  defp data_type_to_elixir(:f8le, _), do: {:float, [], []}
  defp data_type_to_elixir(:f8be, _), do: {:float, [], []}

  defp data_type_to_elixir(:str, _),
    do:
      {{:., [],
        [
          {:__aliases__, [alias: false],
           [:KaitaiToolkitTest, :Formats, :DnsPacketTest, :DnsPacket, :PointerStruct]},
          :t
        ]}, [], []}

  defp data_type_to_elixir(:strz, _),
    do: {{:., [], [{:__aliases__, [alias: false], [:String]}, :t]}, [], []}

  defp data_type_to_elixir(:bytes, _), do: {:binary, [], []}
  defp data_type_to_elixir(:bool, _), do: {:boolean, [], []}
  defp data_type_to_elixir(:struct, _), do: {:map, [], []}

  defp data_type_to_elixir(:io, _),
    do: {{:., [], [{:__aliases__, [alias: false], [:KaitaiStruct, :Stream]}, :t]}, [], []}

  defp data_type_to_elixir(:any, _), do: {:term, [], []}
  defp data_type_to_elixir({:bits, _}, _), do: :integer

  defp data_type_to_elixir({:user_defined, name}, prefix),
    do:
      {{:., [],
        [
          {:__aliases__, [alias: false],
           Enum.map(prefix, &String.to_atom/1) ++ [String.to_atom(Macro.camelize(name))]},
          :t
        ]}, [], []}

  defp data_type_to_elixir(%{switch_on: _, cases: _}, _), do: {:term, [], []}
end

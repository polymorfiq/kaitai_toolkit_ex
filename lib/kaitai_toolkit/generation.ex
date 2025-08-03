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
        abs_mod_name = Module.concat(opts[:root], mod_name)

        quote do
          defmodule unquote(abs_mod_name) do
            unquote_splicing(gen_aliases(mod))

            unquote(gen_module_defstruct(mod))
            unquote(gen_module_typespec(mod))

            unquote_splicing(gen_read_functions(mod))
          end
        end
      end)

    quote do
      unquote_splicing(gen_aliases(root_module))

      unquote_splicing(nested_modules)

      unquote(gen_module_defstruct(root_module))
      unquote(gen_module_typespec(root_module))

      unquote_splicing(gen_read_functions(root_module))
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

  defp gen_read_functions(_mod) do
    read_spec = quote do: @spec(read!(io :: KaitaiStream.t()) :: t())

    read_def =
      quote do
        def read!(io) do
          %__MODULE__{}
        end
      end

    [read_spec, read_def]
  end

  defp extract_modules(ksy, opts) do
    modules =
      ksy.types
      |> Enum.map(fn {type_id, type} ->
        mod_name = :"#{Module.concat(opts[:namespace], Macro.camelize(type_id))}"
        mod_def = type_to_module(type, opts)

        {mod_name, mod_def}
      end)

    nested_modules =
      ksy.types
      |> Enum.flat_map(fn {type_id, type} ->
        namespace = Module.concat(opts[:namespace], Macro.camelize(type_id))
        processed = extract_modules(type, opts ++ [namespace: namespace])

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
            [:"#{Module.concat(opts[:namespace], Macro.camelize(name))}"]

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

defmodule KaitaiToolkit.Generation do
  alias KaitaiToolkit.Generation.Attribute
  alias KaitaiToolkit.Generation.Module, as: KaitaiModule

  defstruct [:attrs, :modules]

  def generate(ksy, opts \\ []) do
    root_module = type_to_module(:root, ksy)
    opts = opts ++ [ksy: ksy]

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
    read_spec =
      quote do
        @spec read!(io :: KaitaiStream.t(), read_opts :: map()) :: t()
      end

    base_map = quote do: %__MODULE__{}

    read_def =
      quote do
        def read!(io, read_opts \\ %{parents: []}) do
          unquote(gen_read_steps(mod, base_map, mod.attrs, opts))
        end
      end

    [read_spec, read_def]
  end

  defp gen_read_steps(_mod, left, [], _opts) do
    quote do
      unquote(left)
    end
  end

  defp gen_read_steps(mod, left, [attr | rest_attrs], opts) do
    _attr_id = String.to_atom(attr.name)
    with_step = gen_read_step(attr, mod, left, opts)

    gen_read_steps(mod, with_step, rest_attrs, opts)
  end

  defp gen_read_step(%{if: {:expr, expr_str}} = attr, mod, left, opts) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        ctx = %{io: io, self: ksy, parents: read_opts.parents}

        if KaitaiToolkit.Struct.parse_expr!(ctx, unquote({:expr, expr_str})) do
          unquote(gen_read_step(%{attr | if: nil}, mod, quote(do: ksy), opts))
        else
          ksy
        end
      end)
    end
  end

  defp gen_read_step(%{data_type: data_type, repeat: :repeat_eos} = attr, _mod, left, opts) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        Map.put(
          ksy,
          unquote(String.to_atom(attr.name)),
          KaitaiStruct.Stream.repeat(io, :eos, fn io, _ ->
            unquote(gen_read_fn(data_type, opts))
          end)
        )
      end)
    end
  end

  defp gen_read_step(
         %{data_type: data_type, repeat: {:repeat_const, num_repeats}} = attr,
         _mod,
         left,
         opts
       ) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        Map.put(
          ksy,
          unquote(String.to_atom(attr.name)),
          Enum.map(Stream.repeatedly(fn -> 1 end) |> Enum.take(unquote(num_repeats)), fn _idx ->
            unquote(gen_read_fn(data_type, opts))
          end)
        )
      end)
    end
  end

  defp gen_read_step(
         %{data_type: data_type, repeat: {:repeat_expr, repeat_expr}} = attr,
         _mod,
         left,
         opts
       ) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        ctx = %{io: io, self: ksy, parents: read_opts.parents}
        repeat_val = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(repeat_expr))

        case repeat_val do
          repeat_count when is_integer(repeat_count) ->
            val =
              Enum.map(Stream.repeatedly(fn -> 1 end) |> Enum.take(repeat_count), fn _idx ->
                unquote(gen_read_fn(data_type, opts))
              end)

            Map.put(ksy, unquote(String.to_atom(attr.name)), val)
        end
      end)
    end
  end

  defp gen_read_step(
         %{data_type: data_type, repeat: {:repeat_until, until_expr}} = attr,
         _mod,
         left,
         opts
       ) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        val =
          Stream.repeatedly(fn -> 1 end)
          |> Enum.reduce_while([], fn _, acc ->
            ctx = %{io: io, self: acc, parents: [ksy | read_opts.parents]}
            until_state = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(until_expr))

            if until_state,
              do: {:halt, Enum.reverse(acc)},
              else: {:cont, [unquote(gen_read_fn(data_type, opts)) | acc]}
          end)

        Map.put(ksy, unquote(String.to_atom(attr.name)), val)
      end)
    end
  end

  defp gen_read_step(%{data_type: :bytes} = attr, _mod, left, _opts) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        ctx = %{io: io, self: ksy, parents: read_opts.parents}
        size = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(attr.attr.size))

        Map.put(
          ksy,
          unquote(String.to_atom(attr.name)),
          KaitaiStruct.Stream.read_bytes_array!(io, size)
        )
      end)
    end
  end

  defp gen_read_step(%{data_type: :io} = attr, _mod, left, _opts),
    do: quote(do: unquote(left) |> Map.put(unquote(String.to_atom(attr.name)), io))

  defp gen_read_step(%{data_type: :str} = attr, _mod, left, _opts) do
    quote do
      unquote(left)
      |> then(fn ksy ->
        ctx = %{io: io, self: ksy, parents: read_opts.parents}
        size = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(attr.attr.size))

        Map.put(
          ksy,
          unquote(String.to_atom(attr.name)),
          KaitaiStruct.Stream.read_bytes_array!(io, size)
        )
      end)
    end
  end

  defp gen_read_step(%{data_type: :strz} = attr, _mod, left, _opts) do
    quote do
      unquote(left)
      |> Map.put(
        unquote(String.to_atom(attr.name)),
        KaitaiStruct.Stream.read_bytes_term!(
          io,
          unquote(attr.attr.encoding || "UTF-8"),
          unquote(0),
          unquote(attr.attr.include),
          unquote(attr.attr.consume),
          unquote(attr.attr.eos_error)
        )
      )
    end
  end

  defp gen_read_step(%{data_type: %{switch_on: switch_on, cases: cases}}, _mod, left, opts) do
    ksy = Keyword.fetch!(opts, :ksy)

    quote do
      unquote(left) |> then(fn ksy ->
        ksy |> IO.inspect(label: "ksy")
        ctx = %{io: io, self: ksy, parents: read_opts.parents}
        switch_val = KaitaiToolkit.Struct.parse_expr!(ctx, {:expr, unquote(switch_on)})

        val_to_type = unquote({:%{}, [], Enum.map(cases, fn {key, val} ->
          switch_key = cond do
            is_number(key) -> key
            is_binary(key) ->
              [enum_name, enum_val_id] = String.split(key, "::")
              {_, curr_enum} = Enum.find(ksy.enums, fn {id, _} -> id == enum_name end)
              curr_vals_by_id = Map.new(curr_enum, fn {val, spec} -> {spec.id, val} end)
              raw_enum_case = Map.fetch!(curr_vals_by_id, enum_val_id)

              raw_enum_case
          end

          {switch_key, quote(do: fn -> unquote(gen_read_fn(val, opts)) end)}
        end)})

        Map.fetch!(val_to_type, switch_val).()
      end)
    end
  end

  defp gen_read_step(%{data_type: data_type} = attr, _mod, left, opts) do
    quote do
      unquote(left)
      |> then(fn ksy -> Map.put(ksy, unquote(String.to_atom(attr.name)), unquote(gen_read_fn(data_type, opts))) end)
    end
  end

  defp gen_read_fn(:u1, _), do: quote(do: KaitaiStruct.Stream.read_u1!(io))
  defp gen_read_fn(:u2, _), do: quote(do: KaitaiStruct.Stream.read_u2be!(io))
  defp gen_read_fn(:u2le, _), do: quote(do: KaitaiStruct.Stream.read_u2le!(io))
  defp gen_read_fn(:u2be, _), do: quote(do: KaitaiStruct.Stream.read_u2be!(io))
  defp gen_read_fn(:u4, _), do: quote(do: KaitaiStruct.Stream.read_u4be!(io))
  defp gen_read_fn(:u4le, _), do: quote(do: KaitaiStruct.Stream.read_u4le!(io))
  defp gen_read_fn(:u4be, _), do: quote(do: KaitaiStruct.Stream.read_u4be!(io))
  defp gen_read_fn(:u8, _), do: quote(do: KaitaiStruct.Stream.read_u8be!(io))
  defp gen_read_fn(:u8le, _), do: quote(do: KaitaiStruct.Stream.read_u8le!(io))
  defp gen_read_fn(:u8be, _), do: quote(do: KaitaiStruct.Stream.read_u8be!(io))
  defp gen_read_fn(:s1, _), do: quote(do: KaitaiStruct.Stream.read_s1!(io))
  defp gen_read_fn(:s2, _), do: quote(do: KaitaiStruct.Stream.read_s2be!(io))
  defp gen_read_fn(:s2le, _), do: quote(do: KaitaiStruct.Stream.read_s2le!(io))
  defp gen_read_fn(:s2be, _), do: quote(do: KaitaiStruct.Stream.read_s2be!(io))
  defp gen_read_fn(:s4, _), do: quote(do: KaitaiStruct.Stream.read_s4be!(io))
  defp gen_read_fn(:s4le, _), do: quote(do: KaitaiStruct.Stream.read_s4le!(io))
  defp gen_read_fn(:s4be, _), do: quote(do: KaitaiStruct.Stream.read_s4be!(io))
  defp gen_read_fn(:s8, _), do: quote(do: KaitaiStruct.Stream.read_s8be!(io))
  defp gen_read_fn(:s8le, _), do: quote(do: KaitaiStruct.Stream.read_s8le!(io))
  defp gen_read_fn(:s8be, _), do: quote(do: KaitaiStruct.Stream.read_s8be!(io))
  defp gen_read_fn(:f4, _), do: quote(do: KaitaiStruct.Stream.read_f4be!(io))
  defp gen_read_fn(:f4le, _), do: quote(do: KaitaiStruct.Stream.read_f4le!(io))
  defp gen_read_fn(:f4be, _), do: quote(do: KaitaiStruct.Stream.read_f4be!(io))
  defp gen_read_fn(:f8, _), do: quote(do: KaitaiStruct.Stream.read_f8be!(io))
  defp gen_read_fn(:f8le, _), do: quote(do: KaitaiStruct.Stream.read_f8le!(io))
  defp gen_read_fn(:f8be, _), do: quote(do: KaitaiStruct.Stream.read_f8be!(io))

  defp gen_read_fn({:user_defined, name}, opts) do
    custom_mod_name = :"#{Module.concat(opts[:root], Macro.camelize(name))}"

    quote do
      unquote(custom_mod_name).read!(io, %{read_opts | parents: [ksy | read_opts.parents]})
    end
  end

  defp extract_modules(ksy, opts) do
    modules =
      ksy.types
      |> Enum.map(fn {type_id, type} ->
        mod_name = :"#{Module.concat(opts[:root], Macro.camelize(type_id))}"
        mod_def = type_to_module(mod_name, type, opts)

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

  def type_to_module(mod_name, type, opts \\ []) do
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
        repeat =
          case attr do
            %{repeat: nil} -> nil
            %{repeat: :eos} -> :repeat_eos
            %{repeat: :expr, repeat_expr: num} when is_integer(num) -> {:repeat_const, num}
            %{repeat: :expr, repeat_expr: expr} -> {:repeat_expr, expr}
            %{repeat: :until, repeat_until: until_expr} -> {:repeat_until, until_expr}
          end

        attr_type =
          if repeat,
            do: [data_type_to_elixir(attr.type, type_root)],
            else: data_type_to_elixir(attr.type, type_root)

        %Attribute{
          name: attr.id,
          elixir_type: attr_type,
          data_type: attr.type,
          if: attr.if,
          repeat: repeat,
          attr: attr
        }
      end)

    instances =
      type.instances
      |> Enum.map(fn {inst_id, attr} ->
        repeat =
          case attr do
            %{repeat: nil} -> nil
            %{repeat: :eos} -> :repeat_eos
            %{repeat: :expr, repeat_expr: num} when is_integer(num) -> {:repeat_const, num}
            %{repeat: :expr, repeat_expr: expr} -> {:repeat_expr, expr}
            %{repeat: :until, repeat_until: until_expr} -> {:repeat_until, until_expr}
          end

        attr_type =
          if repeat,
             do: [data_type_to_elixir(attr.type, type_root)],
             else: data_type_to_elixir(attr.type, type_root)

         {inst_id, %Attribute{
          name: attr.id,
          elixir_type: attr_type,
          data_type: attr.type,
          if: attr.if,
          repeat: repeat,
          attr: attr
        }}
      end)

    %KaitaiModule{name: mod_name, attrs: attrs, instances: instances, deps: deps}
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

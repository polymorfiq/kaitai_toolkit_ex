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
      defstruct unquote(mod.attrs |> Enum.map(& &1.name))
    end
  end

  defp gen_module_typespec(mod) do
    type_spec = {:%{}, [], Enum.map(mod.attrs, &{&1.name, &1.elixir_type})}
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
    base_with_instances = gen_instance_steps(mod, base_map, mod.instances, opts)

    read_def =
      quote do
        def read!(io, read_opts \\ %{parents: []}) do
          unquote(gen_read_steps(mod, base_with_instances, mod.attrs, opts))
        end
      end

    attr_functions =
      mod.attrs
      |> Enum.flat_map(fn attr ->
        fn_def =
          quote do
            defp unquote(:"read_#{attr.name}!")(io, ksy, read_opts) do
              ctx = %{io: io, self: ksy, parents: read_opts.parents}

              get_val = fn ->
                unquote(gen_attrib_read_fn(attr, %{root: opts[:root], ksy: opts[:ksy]}))
              end

              Map.put(ksy, unquote(attr.name), get_val.())
            end
          end

        [fn_def]
      end)

    instance_functions =
      mod.instances
      |> Enum.flat_map(fn attr ->
        fn_def =
          quote do
            defp unquote(:"instance_#{attr.name}!")(io, ksy, read_opts) do
              ctx = %{io: io, self: ksy, parents: read_opts.parents}

              get_val = fn ->
                unquote(gen_attrib_read_fn(attr, %{root: opts[:root], ksy: opts[:ksy]}))
              end

              Map.put(ksy, unquote(attr.name), get_val.())
            end
          end

        [fn_def]
      end)

    [read_spec, read_def] ++ attr_functions ++ instance_functions
  end

  defp gen_attrib_read_fn(%{if: {:expr, expr_str}} = attr, attrib_ctx) do
    quote do
      if KaitaiToolkit.Struct.parse_expr!(ctx, unquote({:expr, expr_str})) do
        unquote(gen_attrib_read_fn(%{attr | if: nil}, attrib_ctx))
      else
        nil
      end
    end
  end

  defp gen_attrib_read_fn(%{repeat: :repeat_eos} = attr, attrib_ctx) do
    quote do
      KaitaiStruct.Stream.repeat(io, :eos, fn io, _ ->
        unquote(gen_attrib_read_fn(%{attr | repeat: nil}, attrib_ctx))
      end)
    end
  end

  defp gen_attrib_read_fn(%{repeat: {:repeat_const, num_repeats}} = attr, attrib_ctx) do
    quote do
      Enum.map(Stream.repeatedly(fn -> 1 end) |> Enum.take(unquote(num_repeats)), fn _idx ->
        unquote(gen_attrib_read_fn(%{attr | repeat: nil}, attrib_ctx))
      end)
    end
  end

  defp gen_attrib_read_fn(%{repeat: {:repeat_expr, repeat_expr}} = attr, attrib_ctx) do
    quote do
      repeat_val = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(repeat_expr))

      Enum.map(Stream.repeatedly(fn -> 1 end) |> Enum.take(repeat_val), fn _idx ->
        unquote(gen_attrib_read_fn(%{attr | repeat: nil}, attrib_ctx))
      end)
    end
  end

  defp gen_attrib_read_fn(
         %{data_type: data_type, repeat: {:repeat_until, until_expr}} = attr,
         attrib_ctx
       ) do
    quote do
      Stream.repeatedly(fn -> 1 end)
      |> Enum.reduce_while([], fn _, acc ->
        ctx = %{ctx | io: io, self: acc, parents: [ksy | read_opts.parents]}

        until_state =
          if Enum.count(acc) > 0,
            do: KaitaiToolkit.Struct.parse_expr!(ctx, unquote(until_expr))

        if until_state,
          do: {:halt, Enum.reverse(acc)},
          else:
            {:cont,
             [
               unquote(
                 gen_attrib_read_fn(%{attr | data_type: data_type, repeat: nil}, attrib_ctx)
               )
               | acc
             ]}
      end)
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{contents: contents}}, _) when is_list(contents) do
    quote do
      expected = unquote(Enum.reduce(contents, <<>>, fn
          bytes, acc when is_binary(bytes) ->
            acc <> <<bytes::binary>>

          {:string, content_str}, acc ->
            acc <> <<content_str::binary>>
      end))

      KaitaiStruct.Stream.ensure_fixed_contents!(io, expected)
      expected
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{contents: contents}}, _) when is_binary(contents) do
    quote do
      expected = unquote(contents)

      KaitaiStruct.Stream.ensure_fixed_contents!(io, expected)
      expected
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{contents: {:string, content_str}}}, _) do
    quote do
      expected = unquote(content_str)
      KaitaiStruct.Stream.ensure_fixed_contents!(io, expected)
      expected
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{size_eos: true}}, _) do
    quote do
      size = trunc(KaitaiStruct.Stream.size(io) - KaitaiStruct.Stream.pos(io))
      KaitaiStruct.Stream.read_bytes_array!(io, size)
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{size: size}}, _) when is_number(size) do
    quote do
      KaitaiStruct.Stream.read_bytes_array!(io, unquote(size))
    end
  end

  defp gen_attrib_read_fn(%{data_type: :bytes, attr: %{size: {:expr, expr_str}}}, _) do
    quote do
      size = KaitaiToolkit.Struct.parse_expr!(ctx, unquote({:expr, expr_str}))
      KaitaiStruct.Stream.read_bytes_array!(io, size)
    end
  end

  defp gen_attrib_read_fn(%{data_type: :io}, _),
    do: quote(do: io)

  defp gen_attrib_read_fn(%{data_type: :str} = attr, _) do
    quote do
      size = KaitaiToolkit.Struct.parse_expr!(ctx, unquote(attr.attr.size))
      KaitaiStruct.Stream.read_bytes_array!(io, size)
    end
  end

  defp gen_attrib_read_fn(%{data_type: :strz} = attr, _) do
    quote do
      KaitaiStruct.Stream.read_bytes_term!(
        io,
        unquote(attr.attr.encoding || "UTF-8"),
        unquote(<<0>>),
        unquote(attr.attr.include),
        unquote(attr.attr.consume),
        unquote(attr.attr.eos_error)
      )
    end
  end

  defp gen_attrib_read_fn(%{data_type: %{switch_on: switch_on, cases: cases}} = attr, attrib_ctx) do
    quote do
      switch_val = KaitaiToolkit.Struct.parse_expr!(ctx, {:expr, unquote(switch_on)})

      val_to_type =
        unquote(
          {:%{}, [],
           Enum.map(cases, fn {key, val} ->
             switch_key =
               cond do
                 is_number(key) ->
                   key

                 is_binary(key) ->
                   [enum_name, enum_val_id] = String.split(key, "::")

                   {_, curr_enum} =
                     Enum.find(attrib_ctx.ksy.enums, fn {id, _} -> id == enum_name end)

                   curr_vals_by_id = Map.new(curr_enum, fn {val, spec} -> {spec.id, val} end)
                   raw_enum_case = Map.fetch!(curr_vals_by_id, enum_val_id)

                   raw_enum_case
               end

             {switch_key,
              quote(
                do: fn -> unquote(gen_attrib_read_fn(%{attr | data_type: val}, attrib_ctx)) end
              )}
           end)}
        )

      Map.fetch!(val_to_type, switch_val).()
    end
  end

  defp gen_attrib_read_fn(%{instance: true, value: {:expr, expr_str}}, _) do
    quote do
      {:value_instance,
       fn ctx ->
         KaitaiToolkit.Struct.parse_expr!(ctx, unquote({:expr, expr_str}))
       end}
    end
  end

  defp gen_attrib_read_fn(%{instance: true, pos: {:expr, expr_str}} = attr, attrib_ctx) do
    quote do
      {:instance,
       fn ctx ->
         pos = KaitaiToolkit.Struct.parse_expr!(ctx, unquote({:expr, expr_str}))
         orig_pos = KaitaiStruct.Stream.pos(io)
         :ok = KaitaiStruct.Stream.seek(io, pos)

         val = unquote(gen_attrib_read_fn(%{attr | pos: nil}, attrib_ctx))
         :ok = KaitaiStruct.Stream.seek(io, orig_pos)

         val
       end}
    end
  end

  defp gen_attrib_read_fn(%{data_type: :u1}, _), do: quote(do: KaitaiStruct.Stream.read_u1!(io))
  defp gen_attrib_read_fn(%{data_type: :u2}, _), do: quote(do: KaitaiStruct.Stream.read_u2be!(io))

  defp gen_attrib_read_fn(%{data_type: :u2le}, _),
    do: quote(do: KaitaiStruct.Stream.read_u2le!(io))

  defp gen_attrib_read_fn(%{data_type: :u2be}, _),
    do: quote(do: KaitaiStruct.Stream.read_u2be!(io))

  defp gen_attrib_read_fn(%{data_type: :u4}, _), do: quote(do: KaitaiStruct.Stream.read_u4be!(io))

  defp gen_attrib_read_fn(%{data_type: :u4le}, _),
    do: quote(do: KaitaiStruct.Stream.read_u4le!(io))

  defp gen_attrib_read_fn(%{data_type: :u4be}, _),
    do: quote(do: KaitaiStruct.Stream.read_u4be!(io))

  defp gen_attrib_read_fn(%{data_type: :u8}, _), do: quote(do: KaitaiStruct.Stream.read_u8be!(io))

  defp gen_attrib_read_fn(%{data_type: :u8le}, _),
    do: quote(do: KaitaiStruct.Stream.read_u8le!(io))

  defp gen_attrib_read_fn(%{data_type: :u8be}, _),
    do: quote(do: KaitaiStruct.Stream.read_u8be!(io))

  defp gen_attrib_read_fn(%{data_type: :s1}, _), do: quote(do: KaitaiStruct.Stream.read_s1!(io))
  defp gen_attrib_read_fn(%{data_type: :s2}, _), do: quote(do: KaitaiStruct.Stream.read_s2be!(io))

  defp gen_attrib_read_fn(%{data_type: :s2le}, _),
    do: quote(do: KaitaiStruct.Stream.read_s2le!(io))

  defp gen_attrib_read_fn(%{data_type: :s2be}, _),
    do: quote(do: KaitaiStruct.Stream.read_s2be!(io))

  defp gen_attrib_read_fn(%{data_type: :s4}, _), do: quote(do: KaitaiStruct.Stream.read_s4be!(io))

  defp gen_attrib_read_fn(%{data_type: :s4le}, _),
    do: quote(do: KaitaiStruct.Stream.read_s4le!(io))

  defp gen_attrib_read_fn(%{data_type: :s4be}, _),
    do: quote(do: KaitaiStruct.Stream.read_s4be!(io))

  defp gen_attrib_read_fn(%{data_type: :s8}, _), do: quote(do: KaitaiStruct.Stream.read_s8be!(io))

  defp gen_attrib_read_fn(%{data_type: :s8le}, _),
    do: quote(do: KaitaiStruct.Stream.read_s8le!(io))

  defp gen_attrib_read_fn(%{data_type: :s8be}, _),
    do: quote(do: KaitaiStruct.Stream.read_s8be!(io))

  defp gen_attrib_read_fn(%{data_type: :f4}, _), do: quote(do: KaitaiStruct.Stream.read_f4be!(io))

  defp gen_attrib_read_fn(%{data_type: :f4le}, _),
    do: quote(do: KaitaiStruct.Stream.read_f4le!(io))

  defp gen_attrib_read_fn(%{data_type: :f4be}, _),
    do: quote(do: KaitaiStruct.Stream.read_f4be!(io))

  defp gen_attrib_read_fn(%{data_type: :f8}, _), do: quote(do: KaitaiStruct.Stream.read_f8be!(io))

  defp gen_attrib_read_fn(%{data_type: :f8le}, _),
    do: quote(do: KaitaiStruct.Stream.read_f8le!(io))

  defp gen_attrib_read_fn(%{data_type: :f8be}, _),
    do: quote(do: KaitaiStruct.Stream.read_f8be!(io))

  defp gen_attrib_read_fn(%{data_type: {:user_defined, name}}, attrib_ctx) do
    custom_mod_name = :"#{Module.concat(attrib_ctx.root, Macro.camelize(name))}"

    quote do
      unquote(custom_mod_name).read!(io, %{read_opts | parents: [ksy | read_opts.parents]})
    end
  end

  defp gen_instance_steps(_mod, left, [], _opts) do
    quote do
      unquote(left)
    end
  end

  defp gen_instance_steps(mod, left, [attr | rest_attrs], opts) do
    with_step =
      quote do
        unquote(left)
        |> then(fn ksy -> unquote(:"instance_#{attr.name}!")(io, ksy, read_opts) end)
      end

    gen_instance_steps(mod, with_step, rest_attrs, opts)
  end

  defp gen_read_steps(_mod, left, [], _opts) do
    quote do
      unquote(left)
    end
  end

  defp gen_read_steps(mod, left, [attr | rest_attrs], opts) do
    with_step =
      quote do
        unquote(left)
        |> then(fn ksy -> unquote(:"read_#{attr.name}!")(io, ksy, read_opts) end)
      end

    gen_read_steps(mod, with_step, rest_attrs, opts)
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
          name: String.to_atom(attr.id),
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

        %Attribute{
          name: inst_id,
          instance: true,
          elixir_type: attr_type,
          data_type: attr.type,
          if: attr.if,
          pos: attr.pos,
          value: attr.value,
          repeat: repeat,
          attr: attr
        }
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

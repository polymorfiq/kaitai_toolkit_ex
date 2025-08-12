defmodule KaitaiToolkit.Ksy do
  @moduledoc "Based on specification from https://doc.kaitai.io/ksy_diagram.html"

  alias KaitaiToolkit.Ksy.Attribute
  alias KaitaiToolkit.Ksy.EnumSpec
  alias KaitaiToolkit.Ksy.Instances
  alias KaitaiToolkit.Ksy.Meta
  alias KaitaiToolkit.Ksy.Param
  alias KaitaiToolkit.Ksy.Type

  defstruct [
    :meta,
    :doc,
    :doc_ref,
    :params,
    :seq,
    :types,
    :instances,
    :enums
  ]

  @type t :: %{
          meta: Meta.t(),
          doc: String.t(),
          doc_ref: [String.t()],
          params: [Param.t()],
          seq: [Attribute.t()],
          types: Type.types(),
          instances: Instances.t(),
          enums: EnumSpec.enums()
        }

  @spec sigil_k(String.t(), keyword()) :: t()
  def sigil_k(spec_str, []), do: from_str!(spec_str)

  @spec from_str!(String.t()) :: [t()]
  def from_str!(spec_str) do
    YamlElixir.read_from_string!(spec_str) |> from_map!() |> with_parsed_expressions!()
  end

  @spec from_map!(map()) :: t()
  def from_map!(spec) do
    %__MODULE__{
      meta: meta(spec["meta"]),
      doc: doc(spec["doc"]),
      doc_ref: doc_ref(spec["doc-ref"]),
      params: params(spec["params"]),
      seq: seq(spec["seq"]),
      types: types(spec["types"]),
      instances: instances(spec["instances"]),
      enums: enums(spec["enums"])
    }
  end

  @spec with_parsed_expressions!(t(), ctx :: map()) ::t()
  def with_parsed_expressions!(ksy, ctx \\ %{path: []}) do
    nested_ctx = Map.put_new(ctx, :ksy, ksy)
    seq_ctx = Map.merge(nested_ctx, %{parent: ksy, path: [:seq | ctx.path]})

    %{ksy | seq: Enum.map(ksy.seq, & Attribute.with_parsed_expressions!(&1, seq_ctx))}
  end

  defp meta(nil), do: nil
  defp meta(meta) when is_map(meta), do: Meta.from_map!(meta)

  defp doc(nil), do: ""
  defp doc(doc) when is_binary(doc), do: doc

  defp doc_ref(nil), do: []
  defp doc_ref(doc_ref) when is_binary(doc_ref), do: [doc_ref]
  defp doc_ref(doc_refs) when is_list(doc_refs), do: doc_refs

  defp params(nil), do: []
  defp params(params) when is_list(params), do: Enum.map(params, &Param.from_map!/1)

  defp seq(nil), do: []
  defp seq(attrs) when is_list(attrs), do: Enum.map(attrs, &Attribute.from_map!/1)

  defp types(nil), do: %{}

  defp types(types) do
    Map.new(types, fn {key, type_def} ->
      {key, Type.from_map!(type_def)}
    end)
  end

  defp instances(nil), do: %{}
  defp instances(inst), do: Instances.from_map!(inst)

  defp enums(nil), do: %{}

  defp enums(enums) do
    Map.new(enums, fn {key, enum_def} ->
      enum =
        Enum.map(enum_def, fn {enum_case, data} ->
          {enum_case, EnumSpec.from_data!(data)}
        end)

      {key, enum}
    end)
  end
end

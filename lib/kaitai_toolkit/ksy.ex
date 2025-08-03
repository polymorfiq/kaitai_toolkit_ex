defmodule KaitaiToolkit.Ksy do
  alias KaitaiToolkit.Ksy.Attribute
  alias KaitaiToolkit.Ksy.Meta
  alias KaitaiToolkit.Ksy.Param

  defstruct [:meta, :doc]

  @type t :: %{meta: Meta.t(), doc: String.t(), doc_ref: [String.t()]}

  @spec sigil_k(String.t(), keyword()) :: t()
  def sigil_k(spec_str, []), do: from_str!(spec_str)

  @spec from_str!(String.t()) :: [t()]
  def from_str!(spec_str) do
    specs = YamlElixir.read_all_from_string!(spec_str)

    Enum.map(specs, fn spec ->
      %{
        meta: meta(spec["meta"]),
        doc: doc(spec["doc"]),
        doc_ref: doc_ref(spec["doc-ref"]),
        params: params(spec["params"]),
        seq: seq(spec["seq"])
      }
    end)
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
end

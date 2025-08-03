defmodule KaitaiToolkit.Ksy.Attribute do
  alias KaitaiToolkit.Ksy.ScalarType

  defstruct [
    :id,
    :doc,
    :doc_ref,
    :contents,
    :type,
    :repeat,
    :repeat_expr,
    :repeat_until,
    :if,
    :size,
    :size_eos,
    :process,
    :enum,
    :encoding,
    :pad_right,
    :terminator,
    :consume,
    :include,
    :eos_error,
    :pos,
    :io,
    :value
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          doc: String.t(),
          doc_ref: [String.t()],
          contents: binary(),
          type: ScalarType.type_ref()
        }

  @spec from_map!(map()) :: t()
  def from_map!(data) when is_map(data) do
    %{
      id: id(data["id"]),
      doc: doc(data["doc"]),
      doc_ref: doc_ref(data["doc-ref"]),
      contents: contents(data["contents"]),
      type: type(data["type"])
    }
  end

  defp id(id) when is_binary(id) do
    if Regex.match?(~r|^[a-z][a-z0-9_]*$|, id) do
      id
    else
      raise "KSY Parsing - params.id (#{id}) does not match expected regex (^[a-z][a-z0-9_]*$)"
    end
  end

  defp type(type) when is_binary(type), do: ScalarType.ref_from_str!(type)

  defp doc(nil), do: ""
  defp doc(doc) when is_binary(doc), do: doc

  defp doc_ref(nil), do: ""
  defp doc_ref(doc_ref) when is_binary(doc_ref), do: [doc_ref]
  defp doc_ref(doc_refs) when is_list(doc_refs), do: doc_refs

  defp contents(nil), do: <<>>
  defp contents(contents) when is_binary(contents), do: contents
end

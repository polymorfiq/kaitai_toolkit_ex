defmodule KaitaiToolkit.Ksy.EnumSpec do
  defstruct [:id, :doc, :doc_ref, :orig_id]

  @type t :: %{id: String.t(), doc: String.t(), doc_ref: [String.t()], orig_id: [String.t()]}
  @type enums :: %{String.t() => %{integer() => t()}}

  @spec from_data!(map()) :: t()
  def from_data!(data) when is_binary(data) do
    %__MODULE__{id: data}
  end

  def from_data!(data) when is_map(data) do
    %__MODULE__{
      id: id(data["id"]),
      doc: doc(data["doc"]),
      doc_ref: doc_ref(data["doc-ref"]),
      orig_id: orig_id(data["-orig-id"])
    }
  end

  defp id(id) when is_binary(id) do
    if Regex.match?(~r|^[a-z][a-z0-9_]*$|, id) do
      id
    else
      raise "KSY Parsing - meta.id (#{id}) does not match expected regex (^[a-z][a-z0-9_]*$)"
    end
  end

  defp doc(nil), do: nil
  defp doc(doc) when is_binary(doc), do: doc

  defp doc_ref(nil), do: []
  defp doc_ref(doc_ref) when is_binary(doc_ref), do: [doc_ref]
  defp doc_ref(doc_refs) when is_list(doc_refs), do: doc_refs

  defp orig_id(nil), do: []
  defp orig_id(orig_id) when is_binary(orig_id), do: [orig_id]
  defp orig_id(orig_ids) when is_list(orig_ids), do: orig_ids
end

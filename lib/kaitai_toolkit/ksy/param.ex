defmodule KaitaiToolkit.Ksy.Param do
  alias KaitaiToolkit.Ksy.ScalarType

  defstruct [:id, :type, :doc, :doc_ref, :enum]

  @type t :: %__MODULE__{
          id: String.t(),
          type: ScalarType.pure(),
          doc: String.t(),
          doc_ref: [String.t()],
          enum: String.t() | nil
        }

  @spec from_map!(map()) :: t()
  def from_map!(data) when is_map(data) do
    %{
      id: id(data["id"]),
      type: type(data["type"]),
      doc: doc(data["doc"]),
      doc_ref: doc_ref(data["doc-ref"]),
      enum: enum(data["enum"])
    }
  end

  defp id(id) when is_binary(id) do
    if Regex.match?(~r|^[a-z][a-z0-9_]*$|, id) do
      id
    else
      raise "KSY Parsing - params.id (#{id}) does not match expected regex (^[a-z][a-z0-9_]*$)"
    end
  end

  defp type(type) when is_binary(type), do: ScalarType.pure_from_str!(type)

  defp doc(nil), do: ""
  defp doc(doc) when is_binary(doc), do: doc

  defp doc_ref(nil), do: ""
  defp doc_ref(doc_ref) when is_binary(doc_ref), do: [doc_ref]
  defp doc_ref(doc_refs) when is_list(doc_refs), do: doc_refs

  defp enum(nil), do: nil
  defp enum(enum) when is_binary(enum), do: enum
end

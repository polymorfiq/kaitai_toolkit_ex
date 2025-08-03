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

  @type repeat :: :eos | :expr | :until

  @type t :: %__MODULE__{
          id: String.t(),
          doc: String.t(),
          doc_ref: [String.t()],
          contents: binary(),
          type: ScalarType.type_ref() | %{switch_on: term(), cases: [tuple()]},
          repeat: repeat() | nil,
          repeat_expr: non_neg_integer() | {:expr, String.t()} | nil,
          if: String.t() | nil,
          size: non_neg_integer() | {:expr, String.t()} | nil,
          process: {:xor, binary()} | {:rol, non_neg_integer()} | {:ror, non_neg_integer()} | :zlib | {:custom_processor, {String.t(), [term()]}}
        }

  @spec from_map!(map()) :: t()
  def from_map!(data) when is_map(data) do
    %{
      id: id(data["id"]),
      doc: doc(data["doc"]),
      doc_ref: doc_ref(data["doc-ref"]),
      contents: contents(data["contents"]),
      type: type(data["type"]),
      repeat: repeat(data["repeat"]),
      repeat_expr: repeat_expr(data["repeat-expr"]),
      if: if(data["if"]),
      size: size(data["size"]),
      size_eos: size_eos(data["size-eos"]),
      process: process(data["process"])
    }
  end

  defp id(id) when is_binary(id) do
    if Regex.match?(~r|^[a-z][a-z0-9_]*$|, id) do
      id
    else
      raise "KSY Parsing - params.id (#{id}) does not match expected regex (^[a-z][a-z0-9_]*$)"
    end
  end

  defp type(nil), do: :bytes
  defp type(type) when is_binary(type), do: ScalarType.ref_from_str!(type)
  defp type(%{"switch-on" => switch_on, "cases" => cases}) do
    cases =
      cases
      |> Enum.map(fn {key, ref_str} -> {key, ScalarType.ref_from_str!(ref_str)} end)

    %{switch_on: switch_on, cases: cases}
  end

  defp doc(nil), do: ""
  defp doc(doc) when is_binary(doc), do: doc

  defp doc_ref(nil), do: ""
  defp doc_ref(doc_ref) when is_binary(doc_ref), do: [doc_ref]
  defp doc_ref(doc_refs) when is_list(doc_refs), do: doc_refs

  defp contents(nil), do: <<>>
  defp contents(contents) when is_binary(contents), do: contents

  defp repeat(nil), do: nil
  defp repeat("eos"), do: :eos
  defp repeat("expr"), do: :expr
  defp repeat("until"), do: :until

  defp repeat_expr(nil), do: nil
  defp repeat_expr(num) when is_integer(num), do: num
  defp repeat_expr(str) when is_binary(str) do
    if Regex.match?(~r|^[0-9]+$|, str) do
      String.to_integer(str)
    else
      {:expr, str}
    end
  end

  defp if(nil), do: nil
  defp if(condition) when is_binary(condition), do: condition

  defp size(nil), do: nil
  defp size(num) when is_integer(num), do: num
  defp size(str) when is_binary(str) do
    if Regex.match?(~r|^[0-9]+$|, str) do
      String.to_integer(str)
    else
      {:expr, str}
    end
  end

  defp size_eos(nil), do: false
  defp size_eos(eos) when is_boolean(eos), do: eos

  defp process(nil), do: nil
  defp process("zlib"), do: :zlib
  defp process(process_str) do
    [[_, fn_name, args_str]] = Regex.scan(~r|([a-z][a-z0-9_\.]*)\((.*)\)|, process_str)

    case fn_name do
      "xor" -> {:xor, parse_process_args(args_str)}
      "rol" -> {:rol, parse_process_args(args_str)}
      "ror" -> {:ror, parse_process_args(args_str)}
      custom -> {:custom_processor, {custom, parse_process_args(args_str)}}
    end
  end

  defp parse_process_args(args) do
    args
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn arg ->
      if Regex.match?(~r|^[0-9]+$|, arg) do
        String.to_integer(arg)
      else
        {:expr, arg}
      end
    end)
  end
end

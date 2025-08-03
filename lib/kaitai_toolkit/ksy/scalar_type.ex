defmodule KaitaiToolkit.Ksy.ScalarType do
  @type pure ::
          :u1
          | :u2
          | :u4
          | :u8
          | :s1
          | :s2
          | :s4
          | :s8
          | {:bits, non_neg_integer()}
          | :f4
          | :f8
          | :bytes
          | :str
          | :bool
          | :struct
          | :io
          | :any
          | {:user_defined, String.t()}

  @type type_ref ::
          pure()
          | :u2le
          | :u2be
          | :u4le
          | :u4be
          | :u8le
          | :u8be
          | :s2le
          | :s2be
          | :s4le
          | :s4be
          | :s8le
          | :s8be
          | :f4le
          | :f4be
          | :f8le
          | :f8be
          | :strz

  @spec pure_from_str!(String.t()) :: pure()
  def pure_from_str!(nil), do: :bytes
  def pure_from_str!("u1"), do: :u1
  def pure_from_str!("u2"), do: :u2
  def pure_from_str!("u4"), do: :u4
  def pure_from_str!("u8"), do: :u8
  def pure_from_str!("s1"), do: :s1
  def pure_from_str!("s2"), do: :s2
  def pure_from_str!("s4"), do: :s4
  def pure_from_str!("s8"), do: :s8
  def pure_from_str!("f4"), do: :f4
  def pure_from_str!("f8"), do: :f8
  def pure_from_str!("bytes"), do: :bytes
  def pure_from_str!("str"), do: :str
  def pure_from_str!("bool"), do: :bool
  def pure_from_str!("struct"), do: :struct
  def pure_from_str!("io"), do: :io
  def pure_from_str!("any"), do: :any

  def pure_from_str!(other) when is_binary(other) do
    if Regex.match?(~r|^b[0-9]+$|, other) do
      "b" <> num_bits = other
      {:bits, String.to_integer(num_bits)}
    else
      {:user_defined, other}
    end
  end

  @spec ref_from_str!(String.t()) :: type_ref()
  def ref_from_str!(nil), do: :bytes
  def ref_from_str!("u1"), do: :u1
  def ref_from_str!("u2"), do: :u2
  def ref_from_str!("u2le"), do: :u2le
  def ref_from_str!("u2be"), do: :u2be
  def ref_from_str!("u4"), do: :u4
  def ref_from_str!("u4le"), do: :u4le
  def ref_from_str!("u4be"), do: :u4be
  def ref_from_str!("u8"), do: :u8
  def ref_from_str!("u8le"), do: :u8le
  def ref_from_str!("u8be"), do: :u8be
  def ref_from_str!("s1"), do: :s1
  def ref_from_str!("s2"), do: :s2
  def ref_from_str!("s2le"), do: :s2le
  def ref_from_str!("s2be"), do: :s2be
  def ref_from_str!("s4"), do: :s4
  def ref_from_str!("s4le"), do: :s4le
  def ref_from_str!("s4be"), do: :s4be
  def ref_from_str!("s8"), do: :s8
  def ref_from_str!("s8le"), do: :s8le
  def ref_from_str!("s8be"), do: :s8be
  def ref_from_str!("f4"), do: :f4
  def ref_from_str!("f4le"), do: :f4le
  def ref_from_str!("f4be"), do: :f4be
  def ref_from_str!("f8"), do: :f8
  def ref_from_str!("f8le"), do: :f8le
  def ref_from_str!("f8be"), do: :f8be
  def ref_from_str!("bytes"), do: :bytes
  def ref_from_str!("str"), do: :str
  def ref_from_str!("strz"), do: :strz
  def ref_from_str!("bool"), do: :bool
  def ref_from_str!("struct"), do: :struct
  def ref_from_str!("io"), do: :io
  def ref_from_str!("any"), do: :any

  def ref_from_str!(other) when is_binary(other) do
    if Regex.match?(~r|^b[0-9]+$|, other) do
      "b" <> num_bits = other
      {:bits, String.to_integer(num_bits)}
    else
      {:user_defined, other}
    end
  end
end

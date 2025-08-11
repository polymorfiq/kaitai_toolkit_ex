defmodule KaitaiToolkit.Ksy.TypeSystem do
  @moduledoc "Given a structure in the AST, provides a type"

  @spec type!(tuple()) :: atom() | tuple()
  def type!(val) do
    {:ok, type} = type(val)
    type
  end

  @spec type(tuple()) :: {:ok, atom() | tuple()} | {:error, term()}
  def type({:literal, int}) when is_integer(int), do: {:ok, :integer}
  def type({:literal, float}) when is_float(float), do: {:ok, :float}
  def type({:string, _}), do: {:ok, :string}
end
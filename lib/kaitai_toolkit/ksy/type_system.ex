defmodule KaitaiToolkit.Ksy.TypeSystem do
  @moduledoc "Given a structure in the AST, provides a type"

  @spec type!(tuple()) :: atom() | tuple()
  def type!(val) do
    {:ok, type} = type(val)
    type
  end

  @spec type(tuple()) :: {:ok, atom() | tuple()} | {:error, term()}
  def type(int) when is_integer(int), do: {:ok, :integer}
  def type(float) when is_float(float), do: {:ok, :float}
  def type({:string, _}), do: {:ok, :string}
  def type({:name, name}), do: {:ok, {:runtime_value, name}}
  def type({:logical_or, _, _}), do: {:ok, :boolean}
  def type({:logical_and, _, _}), do: {:ok, :boolean}
  def type({:logical_not, _}), do: {:ok, :boolean}
  def type({:equals, _, _}), do: {:ok, :boolean}
  def type({:not_equals, _, _}), do: {:ok, :boolean}
  def type({:greater_than, _, _}), do: {:ok, :boolean}
  def type({:less_than, _, _}), do: {:ok, :boolean}
  def type({:gt_or_equals, _, _}), do: {:ok, :boolean}
  def type({:lt_or_equals, _, _}), do: {:ok, :boolean}

  @math_ops [:multiply, :divide, :add, :subtract]
  def type({math_op, a, b}) when math_op in @math_ops do
    with {:ok, a_type} <- type(a),
         {:ok, b_type} <- type(b) do
      cond do
        a_type == b_type ->
          {:ok, a_type}

        a_type == :integer && b_type == :float ->
          {:ok, :float}

        a_type == :float && b_type == :integer ->
          {:ok, :float}

        match?({:runtime_value, _}, a_type) && b_type == :integer ->
          {:ok, :integer}

        a_type == :integer && match?({:runtime_value, _}, b_type) ->
          {:ok, :integer}

        match?({:runtime_value, _}, a_type) && b_type == :float ->
          {:ok, :float}

        a_type == :float && match?({:runtime_value, _}, b_type) ->
          {:ok, :float}

        true ->
          {:error, "Non-homogenous #{math_op} types - #{inspect(a_type)} -> #{inspect(b_type)}"}
      end
    else
      {:error, err} -> {:error, err}
    end
  end

  def type(items) when is_list(items) do
    Enum.reduce_while(items, {:ok, {:array, nil}}, fn
      item, {:ok, {:array, nil}} ->
        with {:ok, item_t} <- type(item) do
          {:cont, {:ok, {:array, item_t}}}
        else
          {:error, err} ->
            {:halt,
             {:error, "Array type inference failed for #{inspect(item)} - #{inspect(err)}"}}
        end

      item, {:ok, {:array, curr_type}} ->
        with {:ok, item_t} <- type(item) do
          if curr_type == item_t,
            do: {:cont, {:ok, {:array, item_t}}},
            else:
              {:halt,
               {:error, "Non-homogenous array type - #{inspect(curr_type)} -> #{inspect(item_t)}"}}
        else
          {:error, err} ->
            {:halt,
             {:error, "Array type inference failed for #{inspect(item)} - #{inspect(err)}"}}
        end
    end)
  end
end

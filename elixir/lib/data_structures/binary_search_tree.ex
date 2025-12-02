defmodule DataStructures.BinarySearchTree do
  @moduledoc """
  Binary search tree implementation.
  Time: O(log n) average, O(n) worst case
  Space: O(n)
  """

  defstruct value: nil, left: nil, right: nil, count: 0

  @type t :: %__MODULE__{
          value: any(),
          left: t() | nil,
          right: t() | nil,
          count: non_neg_integer()
        }

  @doc "Create an empty tree"
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Create from list"
  @spec from_list(list()) :: t()
  def from_list(list) do
    Enum.reduce(list, new(), &insert(&2, &1))
  end

  @doc "Insert a value"
  @spec insert(t(), any()) :: t()
  def insert(%__MODULE__{count: 0}, value) do
    %__MODULE__{value: value, count: 1}
  end

  def insert(%__MODULE__{value: node_value, left: left, right: right, count: count} = tree, value) do
    cond do
      value < node_value ->
        %{tree | left: insert(left || new(), value), count: count + 1}

      value > node_value ->
        %{tree | right: insert(right || new(), value), count: count + 1}

      true ->
        %{tree | value: value}
    end
  end

  @doc "Check if value exists"
  @spec contains?(t(), any()) :: boolean()
  def contains?(%__MODULE__{count: 0}, _value), do: false

  def contains?(%__MODULE__{value: node_value, left: left, right: right}, value) do
    cond do
      value < node_value -> contains?(left || new(), value)
      value > node_value -> contains?(right || new(), value)
      true -> true
    end
  end

  @doc "Remove a value"
  @spec remove(t(), any()) :: t()
  def remove(%__MODULE__{count: 0} = tree, _value), do: tree

  def remove(%__MODULE__{value: node_value, left: left, right: right} = tree, value) do
    cond do
      value < node_value ->
        new_left = if left, do: remove(left, value), else: nil
        new_count = count_nodes(new_left) + count_nodes(right) + 1
        %{tree | left: new_left, count: new_count}

      value > node_value ->
        new_right = if right, do: remove(right, value), else: nil
        new_count = count_nodes(left) + count_nodes(new_right) + 1
        %{tree | right: new_right, count: new_count}

      true ->
        remove_node(tree)
    end
  end

  defp remove_node(%__MODULE__{left: nil, right: nil}), do: new()
  defp remove_node(%__MODULE__{left: nil, right: right}), do: right
  defp remove_node(%__MODULE__{left: left, right: nil}), do: left

  defp remove_node(%__MODULE__{left: left, right: right}) do
    {:ok, successor} = find_min(right)
    new_right = remove(right, successor)
    new_count = count_nodes(left) + count_nodes(new_right) + 1
    %__MODULE__{value: successor, left: left, right: new_right, count: new_count}
  end

  defp count_nodes(nil), do: 0
  defp count_nodes(%__MODULE__{count: count}), do: count

  @doc "Find minimum value"
  @spec find_min(t()) :: {:ok, any()} | {:error, :empty}
  def find_min(%__MODULE__{count: 0}), do: {:error, :empty}
  def find_min(%__MODULE__{value: value, left: nil}), do: {:ok, value}
  def find_min(%__MODULE__{left: left}), do: find_min(left)

  @doc "Find maximum value"
  @spec find_max(t()) :: {:ok, any()} | {:error, :empty}
  def find_max(%__MODULE__{count: 0}), do: {:error, :empty}
  def find_max(%__MODULE__{value: value, right: nil}), do: {:ok, value}
  def find_max(%__MODULE__{right: right}), do: find_max(right)

  @doc "Check if tree is empty"
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{count: 0}), do: true
  def empty?(%__MODULE__{}), do: false

  @doc "Get number of nodes"
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{count: count}), do: count

  @doc "Inorder traversal (sorted order)"
  @spec inorder(t()) :: list()
  def inorder(%__MODULE__{count: 0}), do: []

  def inorder(%__MODULE__{value: value, left: left, right: right}) do
    inorder(left || new()) ++ [value] ++ inorder(right || new())
  end

  @doc "Preorder traversal"
  @spec preorder(t()) :: list()
  def preorder(%__MODULE__{count: 0}), do: []

  def preorder(%__MODULE__{value: value, left: left, right: right}) do
    [value] ++ preorder(left || new()) ++ preorder(right || new())
  end

  @doc "Postorder traversal"
  @spec postorder(t()) :: list()
  def postorder(%__MODULE__{count: 0}), do: []

  def postorder(%__MODULE__{value: value, left: left, right: right}) do
    postorder(left || new()) ++ postorder(right || new()) ++ [value]
  end

  @doc "Level order traversal (breadth-first)"
  @spec level_order(t()) :: list()
  def level_order(%__MODULE__{count: 0}), do: []

  def level_order(tree) do
    do_level_order([tree], [])
  end

  defp do_level_order([], acc), do: Enum.reverse(acc)

  defp do_level_order([%__MODULE__{count: 0} | rest], acc) do
    do_level_order(rest, acc)
  end

  defp do_level_order([%__MODULE__{value: value, left: left, right: right} | rest], acc) do
    children =
      [left, right]
      |> Enum.filter(&(&1 != nil && &1.count > 0))

    do_level_order(rest ++ children, [value | acc])
  end

  @doc "Convert to sorted list"
  @spec to_list(t()) :: list()
  def to_list(tree), do: inorder(tree)
end

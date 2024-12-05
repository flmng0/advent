defmodule Advent.Year2024.Day5 do
  @behaviour Advent.Solver

  def parse(input) do
    [ordering, updates] = Advent.chunks(input)

    ordering =
      for line <- ordering, reduce: %{} do
        out ->
          [l, r] =
            line
            |> String.split("|")
            |> Enum.map(&Advent.parse_int!/1)

          Map.update(out, l, [r], &[r | &1])
      end

    updates =
      for line <- updates do
        line
        |> String.split(",")
        |> Enum.map(&Advent.parse_int!/1)
      end

    %{ordering: ordering, updates: updates}
  end

  defp ordered?(x, y, ordering) do
    if afters = ordering[x] do
      y not in afters
    else
      true
    end
  end

  defp unordered?(x, y, ordering), do: not ordered?(x, y, ordering)

  @spec test_update(list(integer()), map()) :: boolean()
  def test_update(update, ordering) do
    update
    |> Enum.with_index()
    |> Enum.all?(fn {x, i} ->
      update
      |> Enum.take(i)
      |> Enum.all?(&ordered?(x, &1, ordering))
    end)
  end

  @spec fix_update(list(integer()), map()) :: list(integer())
  def fix_update(update, ordering) do
    for {x, i} <- Enum.with_index(update), reduce: update do
      update ->
        {l, [_ | r]} = Enum.split(update, i)

        problem_idx = Enum.find_index(l, &unordered?(x, &1, ordering))

        if problem_idx do
          {l, r} = Enum.split(l ++ r, problem_idx)
          l ++ [x | r]
        else
          update
        end
    end
  end

  @impl Advent.Solver
  def solve_a(input) do
    %{ordering: ordering, updates: updates} = parse(input)

    for update <- updates, test_update(update, ordering), reduce: 0 do
      total ->
        center = Enum.at(update, Integer.floor_div(length(update), 2))
        total + center
    end
  end

  @impl Advent.Solver
  def solve_b(input) do
    %{ordering: ordering, updates: updates} = parse(input)

    for update <- updates, not test_update(update, ordering), reduce: 0 do
      total ->
        center_idx = Integer.floor_div(length(update), 2)

        center =
          update
          |> fix_update(ordering)
          |> Enum.at(center_idx)

        total + center
    end
  end
end

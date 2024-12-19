defmodule Advent.Year2024.Day9 do
  @behaviour Advent.Solver

  def parse(input) do
    nums =
      input
      |> String.trim()
      |> String.split("", trim: true)
      |> Enum.map(&Advent.parse_int!/1)

    {blocks, spaces, _bi, _i, _stage} =
      for x <- nums, reduce: {%{}, [], 0, 0, :block} do
        {blocks, spaces, bi, i, :block} ->
          blocks = for j <- 0..(x - 1), into: blocks, do: {j + i, bi}
          {blocks, spaces, bi + 1, i + x, :space}

        {blocks, spaces, bi, i, :space} ->
          new_spaces = for j <- 0..(x - 1), do: i + j
          {blocks, spaces ++ new_spaces, bi, i + x, :block}
      end

    %{blocks: blocks, spaces: spaces}
  end

  def compact(%{blocks: blocks, spaces: spaces}),
    do: compact(blocks, spaces, Enum.reverse(blocks))

  def compact(blocks, [si | spaces], [{i, bi} | rest]) when si < i do
    blocks = Map.put(blocks, si, bi) |> Map.delete(i)
    compact(blocks, spaces, rest)
  end

  def compact(blocks, _, _), do: blocks

  def checksum(blocks) do
    blocks
    |> Enum.map(fn {i, bi} -> i * bi end)
    |> Enum.sum()
  end

  def solve_a(input) do
    map = parse(input)
    compacted = compact(map)

    checksum(compacted)
  end
end

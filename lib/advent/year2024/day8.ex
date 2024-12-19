defmodule Advent.Year2024.Day8 do
  @behaviour Advent.Solver

  def parse(input, combine \\ false) do
    grid = Advent.CharGrid.parse(input)

    nodes =
      grid.chars
      |> Enum.filter(fn
        {_pos, ?.} -> false
        {_pos, _} -> true
      end)
      |> Enum.reduce(%{}, fn {pos, c}, acc ->
        c = if combine, do: ?\s, else: c
        Map.update(acc, c, [pos], &[pos | &1])
      end)

    grid = Map.from_struct(grid)
    Map.put(grid, :nodes, nodes)
  end

  # Used for testing in the REPL
  def render(grid, antinodes) do
    for y <- 0..(grid.height - 1), x <- 0..(grid.width - 1) do
      pos = {x, y}

      char =
        cond do
          grid.chars[pos] != ?. ->
            grid.chars[pos]

          MapSet.member?(antinodes, pos) ->
            "#"

          true ->
            "."
        end

      if x == grid.width - 1 do
        char <> "\n"
      else
        char
      end
    end
    |> List.flatten()
    |> List.to_string()
  end

  def to_edge(w, h, {x, y}, {dx, dy}, acc \\ []) do
    nx = x - dx
    ny = y - dy

    if x >= 0 and x < w and y >= 0 and y < h do
      to_edge(w, h, {nx, ny}, {dx, dy}, [{x, y} | acc])
    else
      acc
    end
  end

  def get_antinodes(%{nodes: nodes, width: w, height: h}, walk \\ false) do
    all =
      for {_, poss} <- nodes do
        antinodes =
          for {x, y} <- poss do
            for {x2, y2} <- poss, x != x2 or y != y2 do
              dx = x2 - x
              dy = y2 - y

              if walk do
                to_edge(w, h, {x, y}, {dx, dy})
              else
                {x - dx, y - dy}
              end
            end
          end

        antinodes
      end

    all
    |> List.flatten()
    |> Enum.filter(fn {x, y} -> x >= 0 and x < w and y >= 0 and y < h end)
    |> MapSet.new()
  end

  def solve_a(input) do
    parse(input) |> get_antinodes() |> MapSet.size()
  end

  def solve_b(input) do
    parse(input) |> get_antinodes(true) |> MapSet.size()
  end
end

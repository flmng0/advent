defmodule Advent.Year2024.Day1 do
  @behaviour Advent.Solver

  def parse(input) do
    parse_line = fn line ->
      String.split(line, " ", trim: true) |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    end

    Advent.map_input(input, parse_line)
    |> Enum.unzip()
  end

  @impl Advent.Solver
  def solve_a(input) do
    parse(input)
    |> Tuple.to_list()
    |> Enum.map(&Enum.sort(&1, :asc))
    |> Enum.zip_with(fn [a, b] -> abs(b - a) end)
    |> Enum.sum()
  end

  @impl Advent.Solver
  def solve_b(input) do
    {l, r} = parse(input)

    hc = Advent.hitcount(r)

    Enum.map(l, fn n ->
      n * Map.get(hc, n, 0)
    end)
    |> Enum.sum()
  end
end
